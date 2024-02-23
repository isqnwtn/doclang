{-# LANGUAGE OverloadedStrings #-}
module Parser.GenericGraph where 

import Prelude hiding (takeWhile)
import Data.ByteString.Char8 (ByteString,pack)
import Data.Attoparsec.ByteString.Char8
        (Parser,takeWhile,inClass,parseOnly
        ,char,string)
import Control.Applicative ((<|>),many)

import Parser.Utils

class Parsable a where 
  parser :: (Parser a)
  def :: a

instance Parsable ByteString where 
  parser = identifier
  def = ""

data G a b = NodeDef !ByteString !a
  | Connection ![ByteString] ![ByteString] !b
  | Subgraph !ByteString ![G a b]
  deriving Show

contentParse :: (Parsable a,Parsable b) => Parser [G a b]
contentParse = many (nodeDefParse <|> connectionParse <|> subgraphParse)
  where
    nodeDefParse = do
      nodename <- ws *> identifier <* ws
      _ <- char '{'
      nodecontent <- ignore *> parser <* ignore
      _ <- char '}' *> ignore
      return $ NodeDef nodename nodecontent

    connectionParse = do
      n1 <- ws *> dotted <* ws
      _ <- string "->"
      n2 <- ws *> dotted <* ignore
      return $ Connection n1 n2 def

    subgraphParse = do
      name <- ws *> string "subgraph" *> ws *> identifier <* ws
      g <- char '{' *> ignore *> contentParse <* ignore <* char '}' <* ignore
      return $ Subgraph name g
