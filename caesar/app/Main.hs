module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "Caesar"
  print $ rot13 "hello"
  print $ caesar 1 "hello"
