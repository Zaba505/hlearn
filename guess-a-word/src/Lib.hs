module Lib
    ( runGame
    ) where

import           Data.List

printChar :: Char -> IO ()
printChar = putStrLn . flip (:) []

maskUnguessedChars :: String -> [Char] -> String
maskUnguessedChars word guesses = reverse $ foldr (mask guesses) [] word
  where
    mask :: [Char] -> Char -> String -> String
    mask guesses letter maskedWord = if elem letter guesses
      then maskedWord ++ [letter] ++ " "
      else maskedWord ++ "_ "

runGame :: [String] -> IO ()
runGame ws = do
  putStrLn $ maskUnguessedChars (ws !! 0) []
  putStrLn "Guess a letter:"
  fmap (\s -> ([], s !! 0)) getLine >>= runGame' (ws !! 0)
  where
    runGame' :: String -> ([Char], Char) -> IO ()
    runGame' word (guesses, guess) = if elem guess guesses
        then do
          putStrLn "already guessed that letter"
          fmap (\s -> ([], s !! 0)) getLine >>= runGame' (ws !! 0)
        else if length (nub word) == length (intersect word guesses)
          then do
            putStrLn $ maskUnguessedChars word (guess:guesses)
            putStrLn "you win"
          else do
            putStrLn $ maskUnguessedChars word (guess:guesses)
            putStrLn "Guess a letter:"
            fmap (\s -> ((guess:guesses), s !! 0)) getLine >>= runGame' word
