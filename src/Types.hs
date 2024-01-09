module Types
  ( MessageContent,
    UserName,
    MessagePack (..),
    Messages,
    ChatBox (..),
    ChatBoxes,
    User (..),
    newUser,
    addMessages,
    readMessages,
  )
where

import Control.Concurrent
import qualified Data.Map as Map

type MessageContent = String

type UserName = String

data MessagePack = MessagePack
  { content :: MessageContent,
    fromUser :: UserName,
    toUser :: UserName
  }
  deriving (Show)

type Messages = [MessageContent]

data ChatBox = ChatBox
  { messages :: Messages,
    readIdx :: Int
  }

type ChatBoxes = Map.Map UserName (MVar ChatBox)

data User = User
  { name :: UserName,
    chatBoxes :: ChatBoxes
  }

newUser :: UserName -> [UserName] -> IO User
newUser name names = do
  chatBoxes <- mapM (\name' -> newMVar ChatBox {messages = [], readIdx = 0} >>= \mvar -> return (name', mvar)) names >>= return . Map.fromList
  return User {name = name, chatBoxes = chatBoxes}

addMessages :: MVar ChatBox -> MessageContent -> IO ()
addMessages chatBoxVar msg = do
  chatBox <- takeMVar chatBoxVar
  putMVar chatBoxVar chatBox {messages = msg : messages chatBox, readIdx = readIdx chatBox + 1}

readMessages :: MVar ChatBox -> IO Messages
readMessages chatBoxVar = do
  box <- readMVar chatBoxVar
  if readIdx box == 0
    then return []
    else do
      box' <- takeMVar chatBoxVar
      let messages' = take (readIdx box') (messages box')
      putMVar chatBoxVar ChatBox {messages = messages box', readIdx = 0}
      return messages'