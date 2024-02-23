{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graph.Graph (createDotOutput) where

import Parser.Parser
import Data.ByteString.Char8 (ByteString, intercalate)

data Node = Node {
   shape :: !(Maybe ByteString)
  ,label :: !ByteString
} deriving Show

data Con = Con
  deriving Show

makeName :: ByteString -> [ByteString] -> ByteString
makeName n ctx = intercalate "_" $ ctx ++ [n]

genDotGraph :: [ByteString] -> G ByteString ByteString -> G Node Con
genDotGraph ctx (NodeDef name c)
  = NodeDef (makeName name ctx) (Node{shape=Nothing,label=c})
genDotGraph ctx (Connection n1 n2 _)
  = Connection (ctx ++ n1)  (ctx ++ n2)  Con
genDotGraph ctx (Subgraph n g) = Subgraph (makeName n ctx) (map (genDotGraph (ctx++[n])) g)

genDot :: [G Node Con] -> ByteString
genDot ls = "digraph root{\n" <> intercalate "\n" (map single ls) <> "\n}"
  where
    single :: G Node Con -> ByteString
    single (NodeDef name Node{..}) = "n_"<>name<>" [label=\""<>label<>"\"]\n"
    single (Connection n1 n2 _) = 
      ("n_"<> intercalate "_" n1) <> " -> " <> ("n_"<>intercalate "_" n2) <> "\n"
    single (Subgraph gname g) = "subgraph " <> gname <> " {\n" <> intercalate "\n" (map single g) <> "\n}\n"

createDotOutput :: Doc ByteString ByteString -> ByteString
createDotOutput Doc{..} = genDot $ map (genDotGraph []) content

