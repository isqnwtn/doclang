{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser (
   Doc(..)
  ,G(..)
  ,dParseEx
  ,parseFile
  ) where

import Prelude hiding (takeWhile)
import System.IO (hGetContents,IOMode(..),openFile)
import Data.ByteString.Char8 (ByteString,pack)
import Data.Attoparsec.ByteString.Char8 
        (Parser,takeWhile,inClass,parseOnly
        ,char,string)
import Control.Applicative ((<|>),many)

data Doc a b = Doc {
    imports :: ![Import]
    , content :: ![G a b]
} deriving Show

data G a b = NodeDef !ByteString !a
  | Connection !ByteString !ByteString !b
  | Subgraph !ByteString ![G a b]
  deriving Show

data Import = Import !ByteString !ByteString
  deriving Show

identifier :: Parser ByteString
identifier = takeWhile $ inClass "A-Za-z0-9_"

ignore :: Parser ByteString
ignore = takeWhile $ inClass " \r\n\t"

ws :: Parser ByteString
ws = takeWhile $ inClass "\r\t "

importParse :: Parser [Import]
importParse = many $ ignore *> singleImport <* ignore
  where 
    singleImport = do 
      _ <- string "@import"
      dest <- ws *> identifier <* ws 
      _ <- string "as"
      alias <- ws *> identifier <* ws
      _ <- ws *> char '\n'
      return $ Import dest alias

contentParse :: Parser [G ByteString ByteString]
contentParse = many (nodeDefParse <|> connectionParse <|> subgraphParse)
  where 
    nodeDefParse = do 
      nodename <- ws *> identifier <* ws
      _ <- char '{'
      nodecontent <- ignore *> identifier <* ignore
      _ <- char '}' *> ignore
      return $ NodeDef nodename nodecontent

    connectionParse = do 
      n1 <- ws *> identifier <* ws 
      _ <- string "->"
      n2 <- ws *> identifier <* ignore
      return $ Connection n1 n2 ""
    subgraphParse = do 
      name <- ws *> string "subgraph" *> ws *> identifier <* ws
      g <- char '{' *> ignore *> contentParse <* ignore <* char '}' <* ignore
      return $ Subgraph name g 



docParse :: Parser (Doc ByteString ByteString)
docParse = Doc <$> (ignore *> importParse <* ignore) <*> ( ignore *> contentParse <* ignore )

parseFile :: String -> IO (Either String (Doc ByteString ByteString))
parseFile filename = do 
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ parseOnly docParse $ pack contents
  

dParseEx :: IO ()
dParseEx = do
  handle <- openFile "data/tsa.doc" ReadMode
  contents <- hGetContents handle
  --putStrLn contents
  print $ parseOnly docParse $ pack contents
