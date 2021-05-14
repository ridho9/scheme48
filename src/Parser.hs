{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char (digitToInt)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric (readHex, readInt, readOct)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf (printf)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String Text
  | Bool Bool
  deriving (Show)

type Parser = Parsec Void Text

symbol :: Parser Char
symbol = oneOf $ T.unpack "!#$%&|*+-/:<=>?@^_~"

stringContentBackslash :: Parser Char
stringContentBackslash =
  (char '\"' >> return '\"')
    <|> (char 'n' >> return '\n')
    <|> (char 'r' >> return '\r')
    <|> (char 't' >> return '\t')
    <|> (char '\\' >> return '\\')

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ (char '\\' >> stringContentBackslash) <|> satisfy (/= '\"')
  char '"'
  return $ String $ T.pack x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> symbol
  rest <- many $ letterChar <|> digitChar <|> symbol
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumberDec :: Parser Integer
parseNumberDec = read <$> some digitChar

parseNumberOct :: Parser Integer
parseNumberOct = do
  [(num, _)] <- readOct <$> (string "#o" >> some octDigitChar)
  return num

parseNumberHex :: Parser Integer
parseNumberHex = do
  [(num, _)] <- readHex <$> (string "#x" >> some hexDigitChar)
  return num

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` T.unpack "01") digitToInt

parseNumberBin :: Parser Integer
parseNumberBin = do
  Just num <- readBin <$> (string "#b" >> some hexDigitChar)
  return num

parseNumber :: Parser LispVal
parseNumber =
  Number
    <$> ( parseNumberDec
            <|> parseNumberOct
            <|> parseNumberHex
            <|> parseNumberBin
        )

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseString <|> parseAtom

-- readExpr :: Text -> Text
readExpr input = case parse parseExpr "lisp" input of
  Left err -> printf "No match: %s" (errorBundlePretty err)
  Right val -> printf "Found value: %s" (show val)
