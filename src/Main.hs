module Main where

import System.Random (StdGen, getStdGen, randomR)

main :: IO ()
main = do
  stdGen <- getStdGen
  gameloop stdGen

gameloop :: StdGen -> IO ()
gameloop stdGen = do
  let (number, stdGen') = randomR (0, 100) stdGen
  guessloop number =<< askNumber
  gameloop stdGen'

askNumber :: IO Int
askNumber = putStr "Guess a number: " >> readLn

guessloop :: Int -> Int -> IO ()
guessloop target guess
  | guess > target = retry target "lower"
  | guess < target = retry target "higher"
  | otherwise = putStrLn "Congratulations!"
                        
retry :: Int -> String -> IO ()
retry target dir = do
  putStr $ "Guess a " ++ dir ++ " number... "
  guessloop target =<< askNumber
