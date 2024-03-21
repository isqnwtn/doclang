{-# LANGUAGE OverloadedStrings #-}
module Parser.GenericGraph where

import Prelude hiding (takeWhile)
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
        (Parser,takeWhile,inClass,parseOnly,skipSpace
        ,char,string)
import Control.Applicative ((<|>),many)

import Parser.Utils

class Parsable a where
  parser :: (Parser a)
  def :: a

instance Parsable ByteString where
  parser = identifier
  def = ""

data ConnPoint = ConnPoint !(Maybe ByteString) ![ByteString]
  deriving Show

data G a b = NodeDef !ByteString !a
  | Connection !ConnPoint !ConnPoint !b
  | Subgraph !ByteString ![G a b]
  deriving Show

contentParse :: (Parsable a,Parsable b) => Parser [G a b]
contentParse = many (nodeDefParse <|> connectionParse <|> subgraphParse)
  where
    nodeDefParse = do
      nodename <- skipSpace *> identifier <* skipSpace
      _ <- char '{'
      nodecontent <- skipSpace *> parser <* skipSpace
      _ <- char '}' *> skipSpace
      return $ NodeDef nodename nodecontent

    connectionParse = do
      n1 <- skipSpace *> readC <* skipSpace
      _ <- string "->"
      n2 <- skipSpace *> readC <* skipSpace
      return $ Connection n1 n2 def
      where
        readC = port <|> noPort
        port = do
          n <- dotted <* char ':'
          p <- identifier
          return $ ConnPoint (Just p) n
        noPort = ConnPoint Nothing <$> dotted

    subgraphParse = do
      name <- skipSpace *> string "subgraph" *> skipSpace *> identifier <* skipSpace
      g <- char '{' *> skipSpace *> contentParse <* skipSpace <* char '}' <* skipSpace
      return $ Subgraph name g
