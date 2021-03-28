module Main where

import Prelude
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.Zipper.ArrayZipper (ArrayZipper, getFocus, shiftFocusFirst, shiftFocusLast, toArrayZipperFirst)
import Data.Zipper.ArrayZipper as Zipper
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HTTPure ((!@))
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Simple.JSON (writeJSON)
import Talon (Action(..), OS(..), commandsFromCaptures, talonExecuteCommand)
import TreeSitter (Capture, Node, Position, Tree, elmLanguage)
import TreeSitter.Cursor (gotoChildAtIndex)
import TreeSitter.Node as Node
import TreeSitter.Parser as Parser
import TreeSitter.Query as Query
import Util (captureFilterByType, zipperToClosest, (~))

foreign import argv :: Array String

queryMapping :: Map String String
queryMapping =
  Map.fromFoldable
    [ "type" ~ "[ (type_declaration (upper_case_identifier) @i) (type_alias_declaration (upper_case_identifier) @i) ] @t"
    , "identifier" ~ "[ (lower_case_identifier) @i (upper_case_identifier) @i ] @t"
    , "function" ~ "(value_declaration (function_declaration_left (lower_case_identifier) @i)) @t"
    , "import" ~ "(import_clause) @t"
    ]

createMatchQuery :: String -> String -> Maybe String
createMatchQuery name match = do
  pattern <- Map.lookup name queryMapping
  Just $ if match == "" then pattern else "(\n" <> pattern <> " " <> "\n(#eq? @i \"" <> match <> "\")\n)"

elmQuerySourceMatch :: String -> String -> String
elmQuerySourceMatch name match = Maybe.fromMaybe "" $ createMatchQuery name match

elmQuerySource :: String
elmQuerySource =
  """
(lower_case_identifier) @identifier 
(upper_case_identifier) @identifier
(function_declaration_left (lower_case_identifier) @function)
(exposed_value) @function
(lower_pattern) @function
(exposed_type (upper_case_identifier) @type)
(type_declaration (upper_case_identifier) @type)
(type_alias_declaration (upper_case_identifier) @type)
"""

-- (union_variant (upper_case_identifier) @constructor)
-- (record_pattern (_ (lower_case_identifier) @record-field))
-- (field_access_expr (lower_case_identifier) @record-field)
-- (field_type name: (lower_case_identifier) @record-field)
-- (field name: (lower_case_identifier) @record-field)
-- (import_clause (upper_case_qid (upper_case_identifier) @module))
-- (import_clause (as_clause (upper_case_identifier) @module-alias))
-- (import_clause (as_clause)) @import-with-alias
--   """
-- (field_type (lower_case_identifier) @record-field)
-- (import_clause (upper_case_qid (upper_case_identifier) @module))
-- (import_clause (as_clause (upper_case_identifier) @module-alias))
-- (import_clause (as_clause)) @import-with-alias
-- (module_declaration (upper_case_qid) @module) 
-- (import_clause (upper_case_qid) @import)
-- (exposed_type (upper_case_identifier) @type)
-- (type_declaration (upper_case_identifier) @type)
-- (type_alias_declaration (upper_case_identifier) @type)
-- (function_declaration_left (lower_case_identifier) @value)
-- (exposed_value) @value
-- (lower_pattern) @value
-- (union_variant (upper_case_identifier) @constroctor)
-- (union_variant) @constroctor
-- | A request to generate commands for a given program
documentQuery :: Ref ServerState -> String -> String -> Position -> HTTPure.ResponseM
documentQuery ref target type_ position = do
  state <- liftEffect $ Ref.read ref
  case state.maybeTree of
    Nothing -> HTTPure.response 400 "No document open"
    Just tree -> do
      -- captures <- liftEffect $ captureFilterByType "t" <$> getCaptures elmLanguage (elmQuerySourceMatch type_ target) elmSource
      query <- liftEffect $ Query.new elmLanguage (elmQuerySourceMatch type_ target)
      captures <- liftEffect $ captureFilterByType "t" <$> Query.captures query tree.rootNode
      let
        maybeZipper = zipperToClosest position <$> toArrayZipperFirst captures
      liftEffect $ Ref.modify_ _ { captureZipper = maybeZipper } ref
      case maybeZipper of
        Nothing -> HTTPure.response 500 "No Captures"
        Just zipper -> HTTPure.response 200 $ writeJSON (getFocus (zipper)).node

documentOpen :: Ref ServerState -> FilePath -> Effect Unit
documentOpen ref file = do
  serverState <- Ref.read ref
  programSource <- readTextFile UTF8 file
  tree <- Parser.parse programSource =<< Parser.new elmLanguage
  Ref.modify_ _ { maybeTree = Just tree } ref
  query <- Query.new elmLanguage elmQuerySource
  captures <- Query.captures query tree.rootNode
  talonExecuteCommand serverState.os $ UpdateSymbols (commandsFromCaptures captures)

documentCycleResult :: Ref ServerState -> String -> HTTPure.ResponseM
documentCycleResult ref direction = do
  serverState <- liftEffect $ Ref.read ref
  case serverState.captureZipper of
    Nothing -> HTTPure.response 400 "No query has been made previously"
    Just zipper -> do
      let
        Tuple move reset = if direction == "backward" then (Tuple Zipper.prev shiftFocusLast) else (Tuple Zipper.next shiftFocusFirst)

        zipper' = move zipper # fromMaybe (reset zipper)
      liftEffect $ Ref.modify_ _ { captureZipper = Just zipper' } ref
      HTTPure.response 200 $ writeJSON (getFocus zipper').node

-- | TODO:
documentSelectParent :: HTTPure.Query -> HTTPure.ResponseM
documentSelectParent query = do
  let
    row = Int.fromString (query !@ "line") <#> (_ - 1)

    column = Int.fromString (query !@ "column") <#> (_ - 1)
  maybeNode <-
    liftEffect case row, column of
      Just r, Just c -> do
        -- programSource <- readTextFile UTF8 (query !@ "file")
        -- tree <- Parser.parse programSource =<< Parser.new elmLanguage
        -- let
        --   node = Node.findByPosition { row: r, column: c } tree.rootNode
        -- logShow node
        -- let
        --   n = Node.parentWithType (query !@ "type") =<< node
        -- logShow n
        pure Nothing
      _, _ -> pure Nothing
  case maybeNode of
    Nothing -> HTTPure.response 500 "not nice"
    Just node -> HTTPure.response 200 $ "" -- writeJSON node 

type ServerState
  = { maybeTree :: Maybe Tree
    , captureZipper :: Maybe (ArrayZipper Capture)
    , os :: OS
    }

-- | A middleware that logs each request
loggingMiddleware :: (HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM
loggingMiddleware router request = do
  liftEffect do
    log "---------------------"
    logShow request
    log "---------------------"
  router request

server :: OS -> HTTPure.ServerM
server os = do
  ref <- Ref.new initialState
  HTTPure.serve 8080 (middlewares $ router ref) $ log "Server now up on port 8080"
  where
  initialState = { maybeTree: Nothing, captureZipper: Nothing, os: os }

  middlewares = loggingMiddleware

  router :: Ref ServerState -> HTTPure.Request -> HTTPure.ResponseM
  router ref request = do
    serversState <- liftEffect $ Ref.read ref
    case request of
      { path: [ "select-parent" ], query } -> documentSelectParent query
      { path: [ "cycle-result" ], query } -> documentCycleResult ref (query !@ "direction")
      { path: [ "document-query" ], query } -> documentQuery ref (query !@ "target") (query !@ "type") (positionFromQuery query # fromMaybe { row: 0, column: 0 })
      { path: [ "document-close" ] } -> do
        liftEffect $ Ref.write initialState ref
        liftEffect $ talonExecuteCommand serversState.os ClearSymbols
        HTTPure.ok ""
      { path: [ "document-open" ], query } -> do
        liftEffect $ documentOpen ref (query !@ "file")
        HTTPure.ok ""
      _ -> HTTPure.notFound

positionFromQuery :: HTTPure.Query -> Maybe Position
positionFromQuery query = do
  let
    row = Int.fromString (query !@ "line")

    column = Int.fromString (query !@ "column")
  case row, column of
    Just r, Just c -> Just { row: r - 1, column: c - 1 }
    _, _ -> Nothing

debug :: Effect Unit
debug = do
  let
    -- q =
    --   """(value_declaration (function_declaration_left (lower_case_identifier) @name)) @declaration (#eq? @name "main")"""
    -- q = """(type_alias_declaration (upper_case_identifier) @name) @t (#eq? @name "Hello")"""
    q = "(field_type name: (lower_case_identifier) @name)"
  programSource <- readTextFile UTF8 "../Example.elm"
  parser <- Parser.new elmLanguage
  tree <- Parser.parse programSource parser
  let
    node = tree.rootNode
  query <- Query.new elmLanguage q
  -- n <- Node.findByPosition { row: 46, column: 14 } node
  -- logShow $ n
  -- matches <- Query.matches query $ node
  -- for_ matches \m -> logShow (m)
  -- captures <- Query.captures query $ node
  testCursor node
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
