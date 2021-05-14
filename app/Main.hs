module Main where

import qualified Data.Text as T
import Parser (readExpr)

main :: IO ()
main = do
  expr <- getLine
  putStrLn $ readExpr $ T.pack expr
