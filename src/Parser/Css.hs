module Parser.Css where

import Data.Char (isDigit, isHexDigit, isSpace)
import qualified Data.Map as Map

newtype CssStyleSheet
  = CssStyleSheet [CssRule]
  deriving (Show)

data CssRule = CssRule
  { selector :: Selector,
    declarations :: Declaration
  }
  deriving (Show)

type Selector = String

type Declaration = Map.Map String Value

data Value = Length Float Unit | ColorValue String deriving (Show)

data Unit = Px deriving (Show)

-- input: "10px"
-- output: (Length 10 Px, "")
parseValue :: String -> (Value, String)
parseValue ('#' : x) = (ColorValue ('#' : color), rest)
  where
    (color, rest) = span isHexDigit x
parseValue input = (Length (read num) Px, removePxFromRest)
  where
    (num, rest) = span isDigit input
    removePxFromRest = drop 2 rest

-- input: "body { color: red; margin: 10px; }"
-- output: (body, " { color: red; margin: 10px; }")
parseSelector :: String -> (Selector, String)
parseSelector input = (trimSpaceSelector, rest)
  where
    (selector, rest) = span (/= '{') input
    trimSpaceSelector = reverse . dropWhile isSpace . reverse $ selector

-- input: "{ color: red; margin: 10px; }"
-- output: (Declaration, String)
parseDeclarations :: String -> (Declaration, String)
parseDeclarations input = undefined

-- input: body { color: red; margin: 10px; }
-- output: (CssRule, String)
-- parseCssRule::String -> (CssRule, String)

-- parseStylesheet::String -> CssStyleSheet
-- input: entire CSS stylesheet as a string
-- output: CssStyleSheet