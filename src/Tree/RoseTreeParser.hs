module Tree.RoseTreeParser (rparseEx) where 

import Prelude hiding (takeWhile)
import System.IO (hGetContents,IOMode(..),openFile)
import Data.ByteString.Char8 (ByteString,pack)
import Data.Attoparsec.ByteString.Char8 
        (Parser,takeWhile,inClass,parseOnly,parse
        ,char,string)
import Control.Applicative ((<|>),many)

data RoseTree a = 
  RNode a
  | RoseTree a [RoseTree a]
  deriving Show

identifier :: Parser ByteString
identifier = takeWhile $ inClass "A-Za-z0-9_"

ignore :: Parser ByteString
ignore = takeWhile $ inClass " \r\n\t"


treeParse :: Parser (RoseTree ByteString)
treeParse =  fullRoseTreeParse <|> nodeParse
  where
    nodeParse = do
      s <- (ignore *> identifier <* ignore)
      return (RNode s)
    fullRoseTreeParse = do
      s <- (ignore *> identifier <* ignore)
      char '{'
      t <- multiParse <|> emptyParse
      char '}'
      return (RoseTree s t)
    emptyParse = do 
     _ <- ignore
     return []
    singleParse = do 
      t <- ignore *> treeParse <* ignore
      return [t]
    multiParse = do 
      t <-  ignore *> many (treeParse <* char ';') <* ignore
      return t


rparseEx :: IO ()
rparseEx = do
  handle <- openFile "data/rtest.doc" ReadMode
  contents <- hGetContents handle
  --putStrLn contents
  print $ parseOnly treeParse $ pack contents
