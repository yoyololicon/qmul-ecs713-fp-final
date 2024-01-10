module LookUp
  ( availableUserName,
    availableMessageContent,
    generateUserNames,
    randomMessage,
  )
where

import System.Random
import System.Random.Shuffle
import System.Random.Stateful (randomM)

availableUserName :: [String]
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

availableMessageContent :: [String]
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

generateUserNames :: Int -> IO [String]
generateUserNames n =
  newStdGen >>= return . take n . shuffle' availableUserName (length availableUserName)

randomMessage :: IO String
randomMessage = do
  gen <- newStdGen
  let (randomMsgIdx, _) = randomR (0, length availableMessageContent - 1) gen :: (Int, StdGen)
  return $ availableMessageContent !! randomMsgIdx