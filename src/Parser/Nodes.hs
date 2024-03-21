{-# LANGUAGE OverloadedStrings #-}
module Parser.Nodes(
  NodeType(..)
  ,Port(..)
  ,Row(..)
)
where 

import Data.ByteString.Char8 (ByteString)
import Control.Applicative ((<|>),many)
import Data.Attoparsec.ByteString.Char8 (char,string,skipSpace, Parser)

import Parser.GenericGraph
import Parser.Utils (identifier,readQuotes)

data Port a b = NoPort !a 
  | WithPort !b !a
  deriving Show

data Row a = Row !a !a
  deriving Show

data NodeType = Quotes !ByteString
  | List ![Port ByteString ByteString]
  | Table ![Row (Port ByteString ByteString)]
  | Empty
  deriving Show

readWithPort :: Parser (Port ByteString ByteString)
readWithPort = hasPort <|> noPort
  where
    hasPort = WithPort <$> (char '(' *> skipSpace *> identifier <* skipSpace)
      <*> (readQuotes <* skipSpace <* char ')' )
    noPort = NoPort <$> readQuotes

instance Parsable NodeType where 
  parser = quoteParse <|> listParse <|> tableParse
    where 
      quoteParse = Quotes <$> readQuotes
      listParse = do 
        _ <- string "list"
        _ <- skipSpace *> char '['
        ls <- many $ skipSpace *> readWithPort <* skipSpace
        _ <- char ']' <* skipSpace
        return $ List ls
      tableParse = do 
        _ <- string "table"
        _ <- skipSpace *> char '['
        ls <- many rowParse 
        _ <- char ']' <* skipSpace
        return $ Table ls
        where 
          rowParse = do 
           _ <- skipSpace *> char '{' <* skipSpace
           v <- Row <$> (skipSpace *> readWithPort) <*> (skipSpace *> readWithPort <* skipSpace)
           _ <- skipSpace *> char '}' <* skipSpace
           pure v

  def = Empty 
