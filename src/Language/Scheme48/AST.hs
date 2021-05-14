module Language.Scheme48.AST where

import Data.Text

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String Text
  | Bool Bool
  | Character Char
  | Float Float
  deriving (Show)
