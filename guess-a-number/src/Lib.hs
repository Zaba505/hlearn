module Lib
    ( runGame
    ) where

import           System.Random
import           Text.Read

readGuess :: (Monad m) => m String -> (String -> m ()) -> m Int
readGuess input printLn = readGuess' =<< fmap readMaybe input
  where
    readGuess' Nothing = do
      printLn "must enter a valid integer."
      readGuess' =<< fmap readMaybe input
    readGuess' (Just n) = return n

runGame :: IO ()
runGame = do
  g <- initStdGen
  runGame' $ fst $ uniformR (1, 10) g
  where
    runGame' ans = do
      putStrLn "Guess a number"
      num <- readGuess getLine putStrLn
      case compare num ans of
        EQ -> putStrLn "Correct!"
        GT -> do
          putStrLn "Too high."
          runGame' ans
        LT -> do
          putStrLn "Too low."
          runGame' ans
