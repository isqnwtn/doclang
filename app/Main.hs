module Main (main) where

import Parser.Parser (parseFile)
import Graph.Graph (createDotOutput)
import Data.ByteString.Char8 (unpack)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    filename:_ -> do
      graph <- parseFile filename
      case graph of 
        Left _ -> return ()
        Right g -> putStrLn $ unpack $ createDotOutput g
    [] -> do
        print "no arguments provided"
  -- graph <- parseFile "data/test.dot"
  -- -- print graph
  -- case graph of 
  --   Left _ -> return ()
  --   Right g -> putStrLn $ unpack $ createDotOutput g
