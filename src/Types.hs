module Types
  ( Message (..),
    MessageContent,
    Messages,
    User (..),
    UserName,
  )
where

import qualified Data.Map as Map

type MessageContent = String

type UserName = String

data Message = Message
  { content :: MessageContent,
    user :: UserName
  }
  deriving (Show)

type Messages = [Message]

type MessageGroups = Map.Map UserName [MessageContent]

data User = User
  { name :: UserName,
    unreadMessages :: Messages,
    readMessages :: MessageGroups
  }
  deriving (Show)