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

receiverProcess :: Chan MessagePack -> ChatBoxes -> IO ()
receiverProcess incomingMessages boxes = forever $ do
  msg <- readChan incomingMessages
  let username = fromUser msg
      msgContent = content msg
      chatBox = boxes Map.! username
  addMessages chatBox msgContent

userProcess :: User -> Chan MessagePack -> IO ()
userProcess user sendChan = forever $ do
  gen <- newStdGen
  let (randomUserIdx, gen') = randomR (0, Map.size (chatBoxes user) - 1) gen :: (Int, StdGen)
      randomUser = Map.keys (chatBoxes user) !! randomUserIdx

  _ <- readMessages (chatBoxes user Map.! randomUser)
  randomMsg <- randomMessage
  writeChan sendChan MessagePack {content = randomMsg, fromUser = name user, toUser = randomUser}

  -- msgs <- mapM readMessages (Map.elems $ chatBoxes user) >>= return . concat

  -- wait for a random time

  let (randomDelay, _) = randomR (1, 1000000) gen' :: (Int, StdGen)
  threadDelay randomDelay

run :: Int -> Int -> Chan MessagePack -> Map.Map UserName (Chan MessagePack) -> IO ()
run _ 0 _ _ = return ()
run c maxNumMsg mainChan userChans = do
  hPutStr stderr $ "\r\ESC[K" ++ "Progress: " ++ show c ++ "/" ++ show maxNumMsg
  if c == maxNumMsg
    then do
      hPutStrLn stderr ""
      return ()
    else do
      msg <- readChan mainChan
      let username = toUser msg
          chatBox = userChans Map.! username
      writeChan chatBox msg
      run (c + 1) maxNumMsg mainChan userChans

main :: IO ()
main = do
  args <- getArgs
  let numUsers = read $ head args :: Int
  let maxNumMsg = read $ args !! 1 :: Int

  gen <- newStdGen
  let userNames = take numUsers $ shuffle' availableUserName (length availableUserName) gen
  users <- mapM (\name -> newUser name userNames) userNames
  userChans <- mapM (\name -> newChan >>= \chan -> return (name, chan)) userNames >>= return . Map.fromList

  mainChan <- newChan :: IO (Chan MessagePack)

  receiverThreadIDs <- mapM (\user -> forkIO $ receiverProcess (userChans Map.! name user) (chatBoxes user)) users
  userThreadIDs <- mapM (\user -> forkIO $ userProcess user mainChan) users

  run 0 maxNumMsg mainChan userChans

  mapM_ killThread $ receiverThreadIDs ++ userThreadIDs

  -- print summary
  putStrLn "Summary:"
  mapM_ (\user -> userSummary user >>= putStrLn) users
