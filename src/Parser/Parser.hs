{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser (
   Doc(..)
  ,G(..)
  ,parseFile
  ) where

import Prelude hiding (takeWhile)
import System.IO (hGetContents,IOMode(..),openFile)
import Data.ByteString.Char8 (ByteString,pack)
import Data.Attoparsec.ByteString.Char8
        (Parser,takeWhile,inClass,parseOnly
        ,char,string,endOfInput, skipSpace)
import Control.Applicative ((<|>),many)

import Parser.Utils
import Parser.GenericGraph
import Parser.Nodes

data Doc a b = Doc {
    imports :: ![Import]
    , content :: ![G a b]
} deriving Show

data Import = Import !ByteString !ByteString
  deriving Show

importParse :: Parser [Import]
importParse = many $ skipSpace *> singleImport <* skipSpace
  where
    singleImport = do
      _ <- string "@import"
      dest <- ws *> identifier <* ws
      _ <- string "as"
      alias <- ws *> identifier <* ws
      _ <- ws *> char '\n'
      return $ Import dest alias

docParse :: Parser (Doc NodeType ByteString)
docParse = Doc <$> iaround importParse <*> iaround contentParse

parseFile :: String -> IO (Either String (Doc NodeType ByteString))
parseFile filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ parseOnly (docParse <* skipSpace) $ pack contents

