{-# LANGUAGE OverloadedStrings #-}

module Language.Scheme48.Parser where

import Data.Char (digitToInt)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Language.Scheme48.AST
import Numeric (readHex, readInt, readOct)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf (printf)

type Parser = Parsec Void Text

symbol :: Parser Char
symbol = oneOf $ T.unpack "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = space1

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
    _ -> Symbol $ T.pack atom

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

parseNumberBin :: Parser Integer
parseNumberBin =
  let readBin = fmap fst . listToMaybe . readInt 2 (`elem` T.unpack "01") digitToInt
   in string "#b" >> some hexDigitChar
        >>= (\(Just n) -> return n) . readBin

parseNumber :: Parser LispVal
parseNumber =
  Number
    <$> ( parseNumberDec
            <|> parseNumberOct
            <|> parseNumberHex
            <|> parseNumberBin
        )

parseNamedChar :: Parser Char
parseNamedChar = (string "space" >> return ' ') <|> (string "newline" >> return '\n')

parseChar :: Parser LispVal
parseChar =
  Character
    <$> ( string "#\\"
            >> label "single char" (notFollowedBy spaceChar >> anySingle)
              <|> parseNamedChar
        )

parseFloat :: Parser LispVal
parseFloat =
  Float . read
    <$> some digitChar <> (T.unpack <$> chunk ".") <> some digitChar

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= (\x -> return $ List [Symbol "quote", x])

parseBacktick :: Parser LispVal
parseBacktick = char '`' >> parseExpr >>= (\x -> return $ List [Symbol "quasiquote", x])

parseUnquote :: Parser LispVal
parseUnquote = char ',' >> parseExpr >>= (\x -> return $ List [Symbol "unquote", x])

parseExpr :: Parser LispVal
parseExpr =
  parseQuoted
    <|> (try parseFloat <|> parseNumber)
    <|> parseString
    <|> parseChar
    <|> parseAtom
    <|> parseBacktick
    <|> parseUnquote
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

-- readExpr :: Text -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> Left $ errorBundlePretty err
  Right val -> Right $ eval val
