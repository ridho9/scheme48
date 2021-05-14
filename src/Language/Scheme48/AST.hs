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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Symbol _) = val
eval val@(Character _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Symbol "quote", val]) = val
eval (List (Symbol func : args)) = apply func args

apply :: Text -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitiveFuncs

primitiveFuncs :: [(Text, [LispVal] -> LispVal)]
primitiveFuncs =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", typeTest "symbol"),
    ("string?", typeTest "string"),
    ("boolean?", typeTest "boolean"),
    ("char?", typeTest "character"),
    ("number?", typeTest "number")
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ unpackNum <$> params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

typeTest :: Text -> [LispVal] -> LispVal
typeTest "symbol" [Symbol _] = Bool True
typeTest "string" [String _] = Bool True
typeTest "number" [Number _] = Bool True
typeTest "boolean" [Bool _] = Bool True
typeTest "character" [Character _] = Bool True
typeTest _ _ = Bool False