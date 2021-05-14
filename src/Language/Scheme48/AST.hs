{-# LANGUAGE OverloadedStrings #-}

module Language.Scheme48.AST where

import Data.Text (Text)
import qualified Data.Text as T

data LispVal
  = Symbol Text
  | Number Integer
  | String Text
  | Bool Bool
  | Character Char
  | Float Float
  | List [LispVal]
  | DottedList [LispVal] LispVal

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> Text
showVal (Symbol s) = s
showVal (Number int) = T.pack $ show int
showVal (String s) = "\"" <> s <> "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "\'" <> T.pack (show c) <> "\'"
showVal (Float v) = T.pack $ show v
showVal (List v) = "(" <> unwordsList v <> ")"
showVal (DottedList h r) = "(" <> unwordsList h <> " . " <> showVal r <> ")"

unwordsList :: [LispVal] -> Text
unwordsList = T.unwords . map showVal
