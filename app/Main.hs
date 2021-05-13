module Main where

import qualified MyLib (someFunc)

name = "Ridho"

main :: IO ()
main = do
  putStrLn "Ridho"
  MyLib.someFunc
