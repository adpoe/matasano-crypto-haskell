module Main where
import System.IO (readFile)
import Data.Time (getCurrentTime)

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn (greet "bobby")
  putStrLn (greet "world")

greet name = "Hello " ++ name ++ "!"

printNumbers = do
  putStrLn (show (3+4))

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

printTime = do
  time <- getCurrentTime
  putStrLn (show time)
