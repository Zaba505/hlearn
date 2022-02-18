module Lib
    ( runGame
    ) where

import           Data.List

printChar :: Char -> IO ()
printChar = putStrLn . flip (:) []

runGame :: [String] -> IO ()
runGame ws = do
  putStrLn "Guess a letter:"
  fmap (\s -> ([], s !! 0)) getLine >>= runGame' (ws !! 0)
  where
    runGame' :: String -> ([Char], Char) -> IO ()
    runGame' word (guesses, guess) = if elem guess guesses
        then do
          putStrLn "already guessed that letter"
          fmap (\s -> ([], s !! 0)) getLine >>= runGame' (ws !! 0)
        else if length (nub word) == length (intersect word guesses)
          then putStrLn "you win"
          else do
            putStrLn "Guess a letter:"
            fmap (\s -> ((guess:guesses), s !! 0)) getLine >>= runGame' word
