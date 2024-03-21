{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graph.Graph (createDotOutput) where

import Prelude hiding (unwords)
import Data.ByteString.Char8 (ByteString, intercalate, pack, unwords)

import Parser.Parser
import Parser.Nodes
import Parser.GenericGraph (ConnPoint(ConnPoint))

data Node = Node {
   shape :: !(Maybe ByteString)
  ,label :: !ByteString
} deriving Show

data Con = Con
  deriving Show

makeName :: ByteString -> [ByteString] -> ByteString
makeName n ctx = intercalate "_" $ ctx ++ [n]


makeNode :: NodeType -> Node
makeNode (Quotes s) = Node{shape=Nothing,label = "\""<>s<>"\""}
makeNode (List ls) = Node{shape=Just "record",label="\"{"<>intercalate " | " (map dispSingle ls)<>"}\""}
  where
    dispSingle (NoPort l) = l
    dispSingle (WithPort p l) = "<"<>p<>"> "<>l
makeNode (Table ls) = Node{shape=Just "none", label="<"<>renderRows ls<>">"}
  where
    renderRows rows = "<TABLE border=\"0\" cellborder=\"1\" cellspacing=\"0\">" 
      <> intercalate "\n" (map renderRow rows) <>"</TABLE>"
    renderRow (Row i j) = "<TR>" <> renderPorted i <> renderPorted j <>"</TR>"
    renderPorted (NoPort v) = "<TD>"<>v<>"</TD>"
    renderPorted (WithPort p v) = "<TD PORT=\""<>p<>"\">"<>v<>"</TD>"

makeNode Empty = Node{shape=Nothing,label = "\"\""}

genDotGraph :: [ByteString] -> G NodeType ByteString -> G Node Con
genDotGraph ctx (NodeDef name c)
  = NodeDef (makeName name ctx) (makeNode c)
genDotGraph ctx (Connection n1 n2 _)
  = Connection (updateConCtx n1)  (updateConCtx n2)  Con
  where updateConCtx (ConnPoint p n) = ConnPoint p (ctx++n)
genDotGraph ctx (Subgraph n g) = Subgraph (makeName n ctx) (map (genDotGraph (ctx++[n])) g)

genDot :: [G Node Con] -> ByteString
genDot ls = "digraph root{\n"
  <> "graph [fontname = \"Agave Nerd Font Mono\"];\n"
  <> "node [shape=rectangle,fontname = \"Agave Nerd Font Mono\"];\n"
  <> intercalate "\n" (map (single 0) ls)
  <> "\n}"
  where
    single :: Int -> G Node Con -> ByteString
    single t (NodeDef name Node{..}) =
      tabs t<>"n_"<>name<>"["<>unwords [nshape,nlabel]<>"]"
      where
        nshape = case shape of
          Just s -> "shape="<>s
          Nothing -> ""
        nlabel = "label="<>label
    single t (Connection n1 n2 _)    =
      tabs t<>displayCon n1 <> " -> " <> displayCon n2
      where displayCon (ConnPoint p n) = "n_"<> intercalate "_" n <> maybe "" (":"<>) p
    single t (Subgraph gname g)      =
      tabs t<>"subgraph " <> gname <> " {\n" <> intercalate "\n" (map (single $ t+1) g) <> "\n}"
    tabs t = pack $ replicate t '\t'

createDotOutput :: Doc NodeType ByteString -> ByteString
createDotOutput Doc{..} = genDot $ map (genDotGraph []) content

