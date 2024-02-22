{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import  Text.Dot 

someFunc :: IO ()
someFunc = putStrLn $ showDot $ g

g :: Dot ()
g = do
  a <- node [("label","a")]
  b <- node [("label","b")]
  c <- node [("label","c")]
  a .->. b
  a .->. c

