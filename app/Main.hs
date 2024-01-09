module Main (main) where

import Control.Concurrent
import Control.Monad (forever)
import qualified Data.Map as Map
import LookUp (availableMessageContent, availableUserName)
import System.Environment
import System.Random
import System.Random.Shuffle
import Types

receiverProcess :: Chan MessagePack -> ChatBoxes -> IO ()
receiverProcess incomingMessages chatBoxes = do
  msg <- readChan incomingMessages
  let username = fromUser msg
      msgContent = content msg
      chatBox = chatBoxes Map.! username
  addMessages chatBox msgContent
  return ()

userProcess :: User -> Chan MessagePack -> IO ()
userProcess user sendChan = forever $ do
  msgs <- mapM readMessages (Map.elems $ chatBoxes user) >>= return . concat
  -- putStrLn $ "User " ++ name user ++ " received " ++ show (length msgs) ++ " messages."

  -- wait for a random time and send a message to a random user
  gen <- newStdGen
  let (randomDelay, gen') = randomR (1, 1000000) gen :: (Int, StdGen)
      (randomUserIdx, gen'') = randomR (0, Map.size (chatBoxes user) - 1) gen' :: (Int, StdGen)
      (randomMsgIdx, _) = randomR (0, length availableMessageContent - 1) gen'' :: (Int, StdGen)
      randomUser = Map.keys (chatBoxes user) !! randomUserIdx
      randomMsg = availableMessageContent !! randomMsgIdx

  -- putStrLn $ name user ++ show randomDelay ++ " " ++ randomUser ++ " " ++ randomMsg

  threadDelay randomDelay

  writeChan sendChan MessagePack {content = randomMsg, fromUser = name user, toUser = randomUser}

-- increment the counter
-- modifyMVar_ msgCounter (\x -> return $ x + 1)
-- userProcess user msgCounter

run :: Int -> Int -> Chan MessagePack -> Map.Map UserName (Chan MessagePack) -> IO ()
run _ 0 _ _ = return ()
run c maxNumMsg mainChan userChans = do
  putStrLn $ "Round " ++ show c
  if c == maxNumMsg
    then return ()
    else do
      msg <- readChan mainChan
      let username = toUser msg
          chatBox = userChans Map.! username
      putStrLn $ "User " ++ username ++ " received message: " ++ content msg
      writeChan chatBox msg
      run (c + 1) maxNumMsg mainChan userChans

main :: IO ()
main = do
  args <- getArgs
  let numUsers = read $ head args :: Int
  let maxNumMsg = read $ args !! 1 :: Int

  gen <- getStdGen
  let userNames = take numUsers $ shuffle' availableUserName (length availableUserName) gen
  users <- mapM (\name -> newUser name userNames) userNames
  userChans <- mapM (\name -> newChan >>= \chan -> return (name, chan)) userNames >>= return . Map.fromList

  mainChan <- newChan :: IO (Chan MessagePack)

  receiverThreadIDs <- mapM (\(chan, user) -> forkIO $ receiverProcess chan (chatBoxes user)) (zip (Map.elems userChans) users)
  userThreadIDs <- mapM (\(user, chan) -> forkIO $ userProcess user chan) (zip users (repeat mainChan))

  run 0 maxNumMsg mainChan userChans

-- -- select random users

-- -- A global counter
-- msgCounter <- newMVar 0

-- users <- mapM (\(name, chan) -> newMVar Map.empty >>= \mvar -> return $ User name chan mvar) $ Map.toList userChans

-- threadIDs <- mapM (\user -> forkIO $ userProcess user userChans msgCounter) users

-- threadDelay 1000000
-- c <- takeMVar msgCounter
-- putStrLn $ "Total number of messages: " ++ show c
