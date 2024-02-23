{-# LANGUAGE OverloadedStrings #-}
module Parser.Nodes(
  NodeType(..)
)
where 

import Data.ByteString.Char8 (ByteString)
import Control.Applicative ((<|>),many)
import Data.Attoparsec.ByteString.Char8 (char,takeTill)

import Parser.GenericGraph
import Parser.Utils (identifier,readQuotes)

data NodeType = Identifier !ByteString  
  | Quotes !ByteString
  | Empty
  deriving Show

instance Parsable NodeType where 
  parser = quoteParse <|> identParse
    where 
      identParse = Identifier <$> identifier
      quoteParse = Quotes <$> readQuotes
  def = Empty 
