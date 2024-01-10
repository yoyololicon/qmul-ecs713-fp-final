module Main (main) where

import Control.Concurrent
import Control.Monad (forever)
import qualified Data.Map as Map
import LookUp
import System.Environment
import System.IO
import System.Random
import System.Random.Shuffle
import Types

type TunnelPack = Either Ack MessagePack

getFromUser :: TunnelPack -> UserName
getFromUser (Left ack) = fromUser . ackDirection $ ack
getFromUser (Right msg) = fromUser . msgDirection $ msg

getToUser :: TunnelPack -> UserName
getToUser (Left ack) = toUser . ackDirection $ ack
getToUser (Right msg) = toUser . msgDirection $ msg

receiverProcess :: Chan TunnelPack -> ChatBoxes -> IO ()
receiverProcess incomingMessages boxes = forever $ do
  pack <- readChan incomingMessages
  let username = getFromUser pack
      chatBox = boxes Map.! username

  case pack of
    Right msg -> addMessage chatBox (content msg)
    Left ack -> theyAcknowledge chatBox (readNum ack)

userProcess :: User -> Chan TunnelPack -> IO ()
userProcess user sendChan = forever $ do
  gen <- newStdGen
  let (randomUserIdx, gen') = randomR (0, Map.size (chatBoxes user) - 1) gen :: (Int, StdGen)
      randomUser = Map.keys (chatBoxes user) !! randomUserIdx

  msgs <- readMessages (chatBoxes user Map.! randomUser)
  writeChan sendChan (Left Ack {readNum = length msgs, ackDirection = (name user, randomUser)})
  randomMsg <- randomMessage
  writeChan sendChan (Right MessagePack {content = randomMsg, msgDirection = (name user, randomUser)})
  addMyMessage (chatBoxes user Map.! randomUser) randomMsg

  -- wait for a random time
  let (randomDelay, _) = randomR (1, 1000000) gen' :: (Int, StdGen)
  threadDelay randomDelay

run :: Int -> Int -> Chan TunnelPack -> Map.Map UserName (Chan TunnelPack) -> IO ()
run _ 0 _ _ = return ()
run c maxNumMsg mainChan userChans = do
  hPutStr stderr $ "\r\ESC[K" ++ "Progress: " ++ show c ++ "/" ++ show maxNumMsg
  if c == maxNumMsg
    then do
      hPutStrLn stderr ""
      return ()
    else do
      pack <- readChan mainChan
      let username = getToUser pack
          chatChan = userChans Map.! username
      writeChan chatChan pack
      case pack of
        Right _ -> run (c + 1) maxNumMsg mainChan userChans
        _ -> run c maxNumMsg mainChan userChans

main :: IO ()
main = do
  args <- getArgs
  let numUsers = read $ head args :: Int
  let maxNumMsg = read $ args !! 1 :: Int

  gen <- newStdGen
  let userNames = take numUsers $ shuffle' availableUserName (length availableUserName) gen
  users <- mapM (\name -> newUser name userNames) userNames
  userChans <- mapM (\name -> newChan >>= \chan -> return (name, chan)) userNames >>= return . Map.fromList

  mainChan <- newChan :: IO (Chan TunnelPack)

  receiverThreadIDs <- mapM (\user -> forkIO $ receiverProcess (userChans Map.! name user) (chatBoxes user)) users
  userThreadIDs <- mapM (\user -> forkIO $ userProcess user mainChan) users

  run 0 maxNumMsg mainChan userChans

  threadDelay 10000
  mapM_ killThread $ receiverThreadIDs ++ userThreadIDs

  -- print summary
  putStrLn "Summary:"
  mapM_ (\user -> userSummary user >>= putStrLn) users
