{-# LANGUAGE OverloadedStrings #-}
module Tree.TreeParser(
  parseEx
)where

import Prelude hiding (takeWhile)
import System.IO (hGetContents,IOMode(..),openFile)
import Data.ByteString.Char8 (ByteString,pack)
import Data.Attoparsec.ByteString.Char8 
        (Parser,takeWhile,inClass,parseOnly
        ,char)
import Control.Applicative ((<|>))

data Tree a = 
  Node a
  | Null
  | Tree a (Tree a) (Tree a)
  deriving Show

identifier :: Parser ByteString
identifier = takeWhile $ inClass "A-Za-z0-9_"

ignore :: Parser ByteString
ignore = takeWhile $ inClass " \r\n\t"

treeParse :: Parser (Tree ByteString)
treeParse =  fullTreeParse <|> nodeParse
  where
    nodeParse = do
      s <- (ignore *> identifier <* ignore)
      return (Node s)
    fullTreeParse = do
      s <- (ignore *> identifier <* ignore)
      char '{'
      t1 <- (ignore *> treeParse <* ignore)
      char ','
      t2 <- (ignore *> treeParse <* ignore)
      char '}'
      return (Tree s t1 t2)

parseEx :: IO ()
parseEx = do
  handle <- openFile "data/test.doc" ReadMode
  contents <- hGetContents handle
  --putStrLn contents
  print $ parseOnly treeParse $ pack contents
