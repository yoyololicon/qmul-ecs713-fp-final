module Types
  ( MessageContent,
    UserName,
    Direction,
    Ack (..),
    MessagePack (..),
    Messages,
    ChatBox (..),
    ChatBoxes,
    User (..),
    fromUser,
    toUser,
    newUser,
    addMessage,
    addMyMessage,
    readMessages,
    theyAcknowledge,
    userSummary,
  )
where

import Control.Concurrent
import qualified Data.Map as Map

type MessageContent = String

type UserName = String

type Direction = (UserName, UserName)

fromUser :: (a, b) -> a
fromUser = fst

toUser :: (a, b) -> b
toUser = snd

data MessagePack = MessagePack
  { content :: MessageContent,
    msgDirection :: Direction
  }
  deriving (Show)

data Ack = Ack
  { readNum :: Int,
    ackDirection :: Direction
  }
  deriving (Show)

type Messages = [MessageContent]

data ChatBox = ChatBox
  { rcvMessages :: Messages,
    sndMessages :: Messages,
    myReadIdx :: Int,
    theirReadIdx :: Int
  }

type ChatBoxes = Map.Map UserName (MVar ChatBox)

data User = User
  { name :: UserName,
    chatBoxes :: ChatBoxes
  }

newUser :: UserName -> [UserName] -> IO User
newUser name names = do
  namesWithoutMe <- return $ filter (/= name) names
  chatBoxes <- mapM (\name' -> newMVar ChatBox {rcvMessages = [], sndMessages = [], myReadIdx = 0, theirReadIdx = 0} >>= \mvar -> return (name', mvar)) namesWithoutMe >>= return . Map.fromList
  return User {name = name, chatBoxes = chatBoxes}

addMessage :: MVar ChatBox -> MessageContent -> IO ()
addMessage chatBoxVar msg = do
  chatBox <- takeMVar chatBoxVar
  putMVar
    chatBoxVar
    chatBox
      { rcvMessages = msg : rcvMessages chatBox,
        sndMessages = sndMessages chatBox,
        myReadIdx = myReadIdx chatBox + 1,
        theirReadIdx = theirReadIdx chatBox
      }

addMyMessage :: MVar ChatBox -> MessageContent -> IO ()
addMyMessage chatBoxVar msg = do
  chatBox <- takeMVar chatBoxVar
  putMVar
    chatBoxVar
    chatBox
      { rcvMessages = rcvMessages chatBox,
        sndMessages = msg : sndMessages chatBox,
        myReadIdx = myReadIdx chatBox,
        theirReadIdx = theirReadIdx chatBox + 1
      }

theyAcknowledge :: MVar ChatBox -> Int -> IO ()
theyAcknowledge chatBoxVar num = do
  chatBox <- takeMVar chatBoxVar
  putMVar
    chatBoxVar
    chatBox
      { rcvMessages = rcvMessages chatBox,
        sndMessages = sndMessages chatBox,
        myReadIdx = myReadIdx chatBox,
        theirReadIdx = theirReadIdx chatBox - num
      }

readMessages :: MVar ChatBox -> IO Messages
readMessages chatBoxVar = do
  box <- readMVar chatBoxVar
  if myReadIdx box == 0
    then return []
    else do
      box' <- takeMVar chatBoxVar
      let messages' = take (myReadIdx box') (rcvMessages box')
      putMVar chatBoxVar ChatBox {rcvMessages = rcvMessages box', sndMessages = sndMessages box', myReadIdx = 0, theirReadIdx = theirReadIdx box'}
      return messages'

userSummary :: User -> IO String
userSummary user = do
  chatBoxes' <- mapM readMVar (chatBoxes user)
  let numRcvMsgs = sum $ map (length . rcvMessages) $ Map.elems chatBoxes'
      numUnreads = sum $ map myReadIdx $ Map.elems chatBoxes'
      numSndMsgs = sum $ map (length . sndMessages) $ Map.elems chatBoxes'
      numAcks = sum $ map theirReadIdx $ Map.elems chatBoxes'
  return $ "User " ++ name user ++ " received " ++ show numRcvMsgs ++ " messages (" ++ show numUnreads ++ " unread) and sent " ++ show numSndMsgs ++ " messages (" ++ show numAcks ++ " has been read)."
