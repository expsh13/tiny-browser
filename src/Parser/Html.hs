module Parser.Html
  ( parse,
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Dom (AttrMap, Node, element, text)

-- | Input type: remaining string to parse
type Input = String

--------------------------------------------------------------------------------
-- Low-level parsing utilities
--------------------------------------------------------------------------------

-- | Peek at the next character without consuming it
-- Returns Nothing if input is empty
nextChar :: Input -> Maybe Char
nextChar "" = Nothing
nextChar (c : _) = Just c

-- | Check if input starts with the given string
startsWith :: Input -> String -> Bool
startsWith "" _ = False
startsWith input str = str `isPrefixOf` input

-- | Check if all input has been consumed
eof :: Input -> Bool
eof "" = True
eof _ = False

-- | Consume and return the first character
-- Precondition: input is not empty
consumeChar :: Input -> (Char, Input)
consumeChar (x : xs) = (x, xs)
consumeChar _ = error "consumeChar: unexpected empty input"

-- | Consume characters while predicate is true
-- Returns (consumed string, remaining input)
consumeWhile :: (Char -> Bool) -> Input -> (String, Input)
consumeWhile = span

--------------------------------------------------------------------------------
-- Simple parsers
--------------------------------------------------------------------------------

-- | Skip prefix whitespace characters
consumeWhitespace :: Input -> Input
consumeWhitespace = dropWhile isSpace

-- | Parse a tag or attribute name (alphanumeric characters)
-- input: "div class=\"foo\">"
-- output: ("div", " class=\"foo\">")
parseName :: Input -> (String, Input)
parseName = consumeWhile isAlphaNum

--------------------------------------------------------------------------------
-- Node parsers
--------------------------------------------------------------------------------

-- | Parse a single node (dispatches to element or text based on '<')
parseNode :: Input -> (Node, Input)
parseNode input@(x : _) = case x of
  '<' -> parseElement input
  _ -> parseText input
parseNode [] = error "parseNode: unexpected end of input"

-- | Parse a text node (consumes until '<' is found)
parseText :: Input -> (Node, Input)
parseText input = (Dom.text textContent, rest)
  where
    (textContent, rest) = consumeWhile (/= '<') input

-- | Parse an element: <tagname attrs>children</tagname>
parseElement :: Input -> (Node, Input)
parseElement ('<' : input) =
  let (tagName, afterName) = parseName input -- Step 1: タグ名
      (attrs, afterAttrs) = parseAttributes afterName -- Step 2: 属性
      (children, afterChildren) = parseNodes afterAttrs -- Step 3: 子ノード
      rest = consumeCloseTag tagName afterChildren -- Step 4: 閉じタグ
   in (element tagName attrs children, rest)
parseElement _ = error "parseElement: expected '<'"

-- | Consume closing tag </tagname>
consumeCloseTag :: String -> Input -> Input
consumeCloseTag tagName input =
  let expected = "</" ++ tagName ++ ">"
      actual = take (length expected) input
   in if actual == expected
        then drop (length expected) input
        else error $ "expected </" ++ tagName ++ "> but got " ++ actual

--------------------------------------------------------------------------------
-- Attribute parsers
--------------------------------------------------------------------------------

-- | Parse all attributes until '>' is found
-- Returns (AttrMap, remaining input after '>')
parseAttributes :: Input -> (AttrMap, Input)
parseAttributes input =
  case consumeWhitespace input of
    ('>' : rest) -> (Map.empty, rest)
    input' ->
      let ((name, value), afterAttr) = parseAttr input'
          (restAttrs, rest) = parseAttributes afterAttr
       in (Map.insert name value restAttrs, rest)

-- | Parse a single attribute: name="value"
parseAttr :: Input -> ((String, String), Input)
parseAttr input = ((name, value), rest2)
  where
    (name, rest1) = parseName input
    (value, rest2) = parseAttrValue $ tail rest1

-- | Parse a quoted attribute value: "value"
parseAttrValue :: Input -> (String, Input)
parseAttrValue (_ : xs) = (value, tail rest)
  where
    (value, rest) = span (/= '"') xs
parseAttrValue _ = error "parseAttrValue: unexpected end of input"

--------------------------------------------------------------------------------
-- Multiple nodes
--------------------------------------------------------------------------------

-- | Parse sibling nodes until end tag or EOF
parseNodes :: Input -> ([Node], Input)
parseNodes input
  | eof input = ([], input) -- 入力終了
  | startsWith input "</" = ([], input) -- 閉じタグ → 終了
  | otherwise =
      let (node, afterNode) = parseNode input -- 1つパース
          (restNodes, rest) = parseNodes afterNode -- 再帰
       in (node : restNodes, rest)

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Parse an HTML document and return the root node
parse :: String -> Node
parse input =
  let (nodes, _) = parseNodes input
   in case nodes of
        [node] -> node -- 1つならそのまま
        _ -> element "html" Map.empty nodes -- 複数なら <html> でラップ
