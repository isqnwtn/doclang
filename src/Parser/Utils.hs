module Parser.Utils (
  identifier
  ,ignore
  ,ws
  ,iaround
  ,dotted
  ,readQuotes
  ,listParse
) where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
        (Parser,takeWhile,takeTill,inClass,parseOnly,skipSpace
        ,char,string, notInClass)
import Data.ByteString.Char8 (ByteString,pack)
import Control.Applicative ((<|>),many)

identifier :: Parser ByteString
identifier = takeWhile $ inClass "A-Za-z0-9_"

ignore :: Parser ByteString
ignore = takeWhile $ inClass " \r\n\t"

ws :: Parser ByteString
ws = takeWhile $ inClass "\r\t "

iaround :: Parser a -> Parser a
iaround p = skipSpace *> p <* skipSpace

readQuotes :: Parser ByteString
readQuotes = char '"' *> takeTill (== '"') <* char '"'

listParse :: Char -> Parser a -> Parser [a]
listParse sep p = single <|> multi
  where
    single = do
      _p <- p
      return [_p]
    multi = do
      h <- many (skipSpace *> p <* skipSpace <* char sep)
      t <- skipSpace *> p <* skipSpace
      return $ h ++[t]

dotted :: Parser [ByteString]
dotted = compound <|> single
  where
    single = do
      s <- identifier
      return [s]
    compound = do
      h <- many $ identifier <* char '.'
      t <- identifier
      return (h ++ [t])
