module Dom (element) where

import qualified Data.Map as Map

type AttrMap = Map.Map String String

data ElementData
  = ElementData
  { tagName :: String,
    attrs :: AttrMap
  }
  deriving (Show)

data NodeType = Text String | Element ElementData deriving (Show)

data Node = Node
  { children :: [Node],
    nodeType :: NodeType
  }
  deriving (Show)

text :: String -> Node
text t = Node {children = [], nodeType = Text t}

element :: String -> AttrMap -> [Node] -> Node
element tag attr children =
  Node
    { children = children,
      nodeType = Element ElementData {tagName = tag, attrs = attr}
    }
