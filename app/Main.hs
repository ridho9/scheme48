module Main where

import qualified Data.Text as T
import Language.Scheme48.Parser (readExpr)
import System.IO (hFlush, stdout)

processLine = do
  expr <- putStr "> " >> hFlush stdout >> getLine
  putStrLn $ case readExpr $ T.pack expr of
    Left err -> "error: " <> err
    Right val -> show val

main :: IO ()
main = do
  processLine
  main
