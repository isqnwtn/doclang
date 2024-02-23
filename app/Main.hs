module Main (main) where

import Parser.Parser (parseFile)
import Graph.Graph (createDotOutput)
import Data.ByteString.Char8 (unpack)

main :: IO ()
main = do 
  graph <- parseFile "data/tsa.dot"
  -- print graph
  case graph of 
    Left _ -> return ()
    Right g -> putStrLn $ unpack $ createDotOutput g
