module Main where

import Prelude
import Data.Array as Array
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (logShow)
import LangaugeConfig as LanguageConfig
import Server (server)
import Talon (OS(..))
import TreeSitter (Capture, Node)
import TreeSitter.Cursor (gotoChildAtIndex)
import TreeSitter.Node as Node
import TreeSitter.Parser as Parser

foreign import argv :: Array String

debug :: Effect Unit
debug = do
  let
    -- q =
    --   """(value_declaration (function_declaration_left (lower_case_identifier) @name)) @declaration (#eq? @name "main")"""
    -- q = """(type_alias_declaration (upper_case_identifier) @name) @t (#eq? @name "Hello")"""
    q = "(field_type name: (lower_case_identifier) @name)"
  -- programSource <- readTextFile UTF8 "../Example.elm"
  parser <- Parser.new LanguageConfig.python.language
  tree <- Parser.parse "x = 5" parser
  let
    node = tree.rootNode
  logShow (Node.toString node)
  -- query <- Query.new elmLanguage q
  -- n <- Node.findByPosition { row: 46, column: 14 } node
  -- logShow $ n
  -- matches <- Query.matches query $ node
  -- for_ matches \m -> logShow (m)
  -- captures <- Query.captures query $ node
  -- testCursor node
  -- testCaptures captures
  pure unit

testCaptures :: Array Capture -> Effect Unit
testCaptures captures = do
  for_ captures \capture -> do
    logShow (Node.toString <$> (Node.parent =<< Node.parent capture.node))

-- logShow $ nodeParentBefore "file" capture.node
testCursor :: Node -> Effect Unit
testCursor node = do
  let
    cursor = Node.walk node
  gotoChildAtIndex 451 cursor
  logShow $ Node.toString cursor.currentNode

-- _ <- gotoFirstChild cursor
-- _ <- gotoFirstChildForIndex 548 cursor
-- _ <- gotoFirstChildForIndex 548 cursor
-- log (cursor.currentNode.text)
-- b <- Cursor.moveUpUntil "value_declaration" cursor
-- _ <- replicateM 1 (gotoNextSibling cursor)
-- _ <- gotoNextSibling cursor
-- logShow (Node.parentWithType "value_declaration" $ cursor.currentNode)
-- logShow $ nodeText <$> nodeFindByPosition { row: 3, column: 4 } node
args :: Array String
args = Array.drop 2 argv

main :: Effect Unit
main = case args of
  [ "debug" ] -> debug
  [ "windows" ] -> server Windows *> pure unit
  _ -> server Unix *> pure unit
