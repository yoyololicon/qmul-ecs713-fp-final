-- |
-- Module      : LookUp
-- Description : Random users and messages generator
--
-- This module contains predefined data and utility functions for chat simulation.
module LookUp
  ( availableUserName,
    availableMessageContent,
    generateUserNames,
    randomMessage,
  )
where

import System.Random
import System.Random.Shuffle
import Types (MessageContent, UserName)

-- | List of available user names
availableUserName :: [UserName]
availableUserName =
  [ "Alice",
    "Bob",
    "Charlie",
    "Dave",
    "Eve",
    "Frank",
    "Grace",
    "Heidi",
    "Ivan",
    "Judy",
    "Mallory",
    "Oscar",
    "Peggy",
    "Sybil",
    "Trent",
    "Walter",
    "Wendy"
  ]

-- | List of available messages
availableMessageContent :: [MessageContent]
availableMessageContent =
  [ "Hello",
    "Hi",
    "How are you?",
    "I'm fine",
    "Goodbye",
    "See you",
    "Good morning",
    "Good afternoon",
    "Good evening",
    "Good night",
    "Good day",
    "Good luck"
  ]

-- |
-- Given a number, generate that many random user names as a list.
-- The number will be capped at the length of `availableUserName`.
generateUserNames :: Int -> IO [UserName]
generateUserNames n =
  newStdGen >>= return . take n . shuffle' availableUserName (length availableUserName)

-- |
-- Generate a random message from `availableMessageContent`.
randomMessage :: IO MessageContent
randomMessage = do
  gen <- newStdGen
  let (randomMsgIdx, _) = randomR (0, length availableMessageContent - 1) gen :: (Int, StdGen)
  return $ availableMessageContent !! randomMsgIdx