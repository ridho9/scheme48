{-# LANGUAGE OverloadedStrings #-}

module Language.Scheme48.Interpreter where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Scheme48.AST
import Language.Scheme48.Parser
import Text.Megaparsec

readExpr input = case parse parseExpr "lisp" input of
  Left err -> Left $ errorBundlePretty err
  Right val -> Right $ eval val

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
