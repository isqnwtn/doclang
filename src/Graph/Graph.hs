{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graph.Graph (createDotOutput) where

import Data.ByteString.Char8 (ByteString, intercalate)

import Parser.Parser
import Parser.Nodes

data Node = Node {
   shape :: !(Maybe ByteString)
  ,label :: !ByteString
} deriving Show

data Con = Con
  deriving Show

makeName :: ByteString -> [ByteString] -> ByteString
makeName n ctx = intercalate "_" $ ctx ++ [n]

genDotGraph :: [ByteString] -> G NodeType ByteString -> G Node Con
genDotGraph ctx (NodeDef name c)
  = NodeDef (makeName name ctx) (Node{shape=Nothing,label=f c})
    where 
      f (Identifier s) = s 
      f (Quotes s)     = s
      f Empty          = ""

genDotGraph ctx (Connection n1 n2 _)
  = Connection (ctx ++ n1)  (ctx ++ n2)  Con
genDotGraph ctx (Subgraph n g) = Subgraph (makeName n ctx) (map (genDotGraph (ctx++[n])) g)

genDot :: [G Node Con] -> ByteString
genDot ls = "digraph root{\n" 
  <> " graph [fontname = \"Agave Nerd Font Mono\"];\n"
  <> " node [shape=rectangle,fontname = \"Agave Nerd Font Mono\"];\n"
  <> intercalate "\n" (map single ls) 
  <> "\n}"
  where
    single :: G Node Con -> ByteString
    single (NodeDef name Node{..}) = "n_"<>name<>" [label=\""<>label<>"\"]\n"
    single (Connection n1 n2 _) = 
      ("n_"<> intercalate "_" n1) <> " -> " <> ("n_"<>intercalate "_" n2) <> "\n"
    single (Subgraph gname g) = "subgraph " <> gname <> " {\n" <> intercalate "\n" (map single g) <> "\n}\n"

createDotOutput :: Doc NodeType ByteString -> ByteString
createDotOutput Doc{..} = genDot $ map (genDotGraph []) content

