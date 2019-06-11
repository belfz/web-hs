module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)
import Data.Aeson (encode)

printTime = do
  time <- getCurrentTime
  putStrLn (show time)

prefix x = "the time is " ++ (show x)

printTime2 = (fmap prefix getCurrentTime) >>= print

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

nums :: [Int]
nums = [1, 2, 3, 4]

printEncodedJ = let
  e = encode nums
  in print e

greet name = "hi " ++ name ++ "!"

repLine :: IO ()
repLine = do
  putStrLn "give me line:"
  line <- getLine
  putStrLn "give num:"
  numStr <- getLine
  print (replicate (read numStr) line)
  

main :: IO ()
main = do
  putStrLn "hello world"
  printTime2
  printEncodedJ
