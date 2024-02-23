{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( testFn
    ) where


import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
        (Parser,takeWhile,takeTill,inClass,parseOnly
        ,char,string, notInClass)
import Data.ByteString.Char8 (ByteString,pack)
import Control.Applicative ((<|>),many)

import Parser.Utils

testFn :: IO ()
testFn = do
  print $ parseOnly par $ "  \n  \"this is a long ass text in quotes\"   "
  where 
    par = ignore *> readQuotes  <* ignore
