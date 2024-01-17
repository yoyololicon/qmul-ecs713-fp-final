module Main (main) where

import Control.Concurrent
import Control.Monad (forever)
import qualified Data.Map as Map
import LookUp
import System.Environment
import System.IO
import System.Random (Random (randomR), StdGen, newStdGen)
import System.Random.Shuffle
import Types

type TunnelPack = Either Ack Message

-- | Get the sender from a `TunnelPack` message
getFromUser :: TunnelPack -> UserName
getFromUser (Left ack) = fromUser . ackDirection $ ack
getFromUser (Right msg) = fromUser . msgDirection $ msg

-- | Get the receiver from a `TunnelPack` message
getToUser :: TunnelPack -> UserName
getToUser (Left ack) = toUser . ackDirection $ ack
getToUser (Right msg) = toUser . msgDirection $ msg

-- | The slave process (receiver) for a user.
-- It reads messages from the message channel and update the chat boxes
receiverProcess :: Chan TunnelPack -> ChatBoxes -> IO ()
receiverProcess incomingMessages boxes = forever $ do
  pack <- readChan incomingMessages
  -- get who sent the message
  let username = getFromUser pack
      chatBox = boxes Map.! username

  case pack of
    -- if it's an message, add it to the chat box
    Right msg -> addMessage chatBox (content msg)
    -- if it's an acknowledgement, update the read number of the sender
    Left ack -> theyAcknowledge chatBox (readNum ack)

-- | The user process for a user.
-- It sends messages to a random user with a random delay (between 0 - 1 second).
-- It also reads any unread messages from that user and acknowledge the number of messages read.
userProcess :: User -> Chan TunnelPack -> IO ()
userProcess user sendChan = forever $ do
  gen <- newStdGen
  let (randomUserIdx, gen') = randomR (0, Map.size (chatBoxes user) - 1) gen :: (Int, StdGen)
      randomUser = Map.keys (chatBoxes user) !! randomUserIdx
      (randomDelay, _) = randomR (1, 1000000) gen' :: (Int, StdGen)

  -- wait for a random time
  threadDelay randomDelay

  msgs <- readMessages (chatBoxes user Map.! randomUser)
  -- send an acknowledgement of the number of messages read
  writeChan sendChan (Left Ack {readNum = length msgs, ackDirection = (name user, randomUser)})
  -- send a random message to the main process
  randomMsg <- randomMessage
  writeChan sendChan (Right Message {content = randomMsg, msgDirection = (name user, randomUser)})
  -- add self message to the chat box
  addMyMessage (chatBoxes user Map.! randomUser) randomMsg

-- | The main process for running the simulation.
-- It reads every message from users and forward them, keep track of the number of messages sent, print the progress, and stop when the number of messages sent reaches the maximum number of messages.
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

      -- forward the message to the user
      writeChan chatChan pack
      case pack of
        -- if it's an message, increment the number of messages sent
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

  -- wait for a while to let the receiver threads process all the tracked messages
  threadDelay 10000
  mapM_ killThread $ receiverThreadIDs ++ userThreadIDs

  -- print summary
  putStrLn "Summary:"
  mapM_ (\user -> userSummary user >>= putStrLn) users
