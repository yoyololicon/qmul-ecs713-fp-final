-- |
-- Module      : Types
-- Description : Data types for chat simulation
--
-- This module contains basic data types for chat simulation and utility functions.
module Types
  ( MessageContent,
    UserName,
    Direction,
    Ack (..),
    Message (..),
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

-- | Get the sender of a message
fromUser :: (a, b) -> a
fromUser = fst

-- | Get the receiver of a message
toUser :: (a, b) -> b
toUser = snd

-- | Data type for sending messages
data Message = Message
  { -- | Message content
    content :: MessageContent,
    -- | Message direction
    msgDirection :: Direction
  }
  deriving (Show)

-- | Data type for acknowledging messages
data Ack = Ack
  { -- | Number of messages read
    readNum :: Int,
    -- | Acknowledgement direction
    ackDirection :: Direction
  }
  deriving (Show)

type Messages = [MessageContent]

-- | Data type for chat boxes
data ChatBox = ChatBox
  { -- | Received messages
    rcvMessages :: Messages,
    -- | Sent messages
    sndMessages :: Messages,
    -- | The `rcvMessages` index of the last message read by the user
    myReadIdx :: Int,
    -- | The `sndMessages` index of the last message read by the other user
    theirReadIdx :: Int
  }

type ChatBoxes = Map.Map UserName (MVar ChatBox)

-- | Data type for users
data User = User
  { -- | User name
    name :: UserName,
    -- | Chat boxes with other users
    chatBoxes :: ChatBoxes
  }

-- | Create a new user with the given name and a list of other users' names
newUser :: UserName -> [UserName] -> IO User
newUser name names = do
  let namesWithoutMe = filter (/= name) names
  chatBoxes <- mapM (\name' -> newMVar ChatBox {rcvMessages = [], sndMessages = [], myReadIdx = 0, theirReadIdx = 0} >>= \mvar -> return (name', mvar)) namesWithoutMe >>= return . Map.fromList
  return User {name = name, chatBoxes = chatBoxes}

-- | Add a received message to the chat box
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

-- | Add a sent message to the chat box
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

-- | Update the chat box when the other user acknowledges messages
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

-- | Read all received messages from the chat box
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

-- | Generate a summary of a user
userSummary :: User -> IO String
userSummary user = do
  chatBoxes' <- mapM readMVar (chatBoxes user)
  let numRcvMsgs = sum $ map (length . rcvMessages) $ Map.elems chatBoxes'
      numUnreads = sum $ map myReadIdx $ Map.elems chatBoxes'
      numSndMsgs = sum $ map (length . sndMessages) $ Map.elems chatBoxes'
      numAcks = numSndMsgs - sum (map theirReadIdx $ Map.elems chatBoxes')
  return $ "User " ++ name user ++ " received " ++ show numRcvMsgs ++ " messages (" ++ show numUnreads ++ " unread) and sent " ++ show numSndMsgs ++ " messages (" ++ show numAcks ++ " has been read)."
