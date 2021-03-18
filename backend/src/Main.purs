module Main where

import Prelude
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.List.Lazy (replicateM)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Data.Zipper.ArrayZipper (ArrayZipper, getFocus, shiftFocusFirst, shiftFocusLast, toArrayZipperFirst)
import Data.Zipper.ArrayZipper as Zipper
import Effect (Effect, whileE)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Ref as Ref
import HTTPure ((!@))
import HTTPure as HTTPure
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Simple.JSON (read_, writeJSON)
import Talon (Action(..), commandsFromCaptures, talonExecuteCommand, talonExecuteUpdate)
import TreeSitter (Capture, Language, Node, Tree, TreeCursor, Position, elmLanguage, json)
import TreeSitter.Cursor (gotoFirstChild, gotoFirstChildForIndex, gotoNextSibling, gotoParent, gotoChildAtIndex)
import TreeSitter.Cursor as Cursor
import TreeSitter.Node as Node
import TreeSitter.Parser as Parser
import TreeSitter.Query as Query
import Util ((~))

foreign import argv :: Array String

queryMapping :: Map String String
queryMapping =
  Map.fromFoldable
    [ "type" ~ "[ (type_declaration (upper_case_identifier) @i) (type_alias_declaration (upper_case_identifier) @i) ] @t"
    -- , "function" ~ "(value_declaration) @i"
    , "identifier" ~ "[ (lower_case_identifier) @i (upper_case_identifier) @i ] @t"
    , "function" ~ "(value_declaration (function_declaration_left (lower_case_identifier) @i)) @t"
    , "import" ~ "(import_clause) @t"
    ]

createMatchQuery :: String -> String -> Maybe String
createMatchQuery name match = do
  pattern <- Map.lookup name queryMapping
  -- TODO: this will find first matched prefix. "Model" will match for "Mode"
  Just $ if match == "" then pattern else "(\n" <> pattern <> " " <> "\n(#eq? @i \"" <> match <> "\")\n)"

elmQuerySourceMatch :: String -> String -> String
elmQuerySourceMatch name match = Maybe.fromMaybe "" $ createMatchQuery name match

-- elmQuerySourceMatch m =
--   """
-- (
--   [ (type_declaration (upper_case_identifier) @i) (type_alias_declaration (upper_case_identifier) @i) ]
--   (#match? @i """
--     <> show m
--     <> """)
-- )
--   """
elmQuerySource :: String
elmQuerySource =
  """
(lower_case_identifier) @identifier 
(upper_case_identifier) @identifier

(function_declaration_left (lower_case_identifier) @function)
(exposed_value) @function
(lower_pattern) @function
(union_variant (upper_case_identifier) @constructor)

(exposed_type (upper_case_identifier) @type)
(type_declaration (upper_case_identifier) @type)
(type_alias_declaration (upper_case_identifier) @type)

(record_pattern (_ (lower_case_identifier) @record-field))
(field_access_expr (lower_case_identifier) @record-field)
(field_type name: (lower_case_identifier) @record-field)
(field name: (lower_case_identifier) @record-field)

(import_clause (upper_case_qid (upper_case_identifier) @module))
(import_clause (as_clause (upper_case_identifier) @module-alias))
(import_clause (as_clause)) @import-with-alias
  """

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
captureFilterByType :: String -> Array Capture -> Array Capture
captureFilterByType typeName = Array.filter $ (_ == typeName) <<< _.name

-- | A request to generate commands for a given program
type Request
  = { language :: Language
    , querySource :: String
    , programSource :: String
    }

getCaptures :: Language -> String -> String -> Effect (Array Capture)
getCaptures language querySource programSource = do
  tree <- Parser.parse programSource =<< Parser.new elmLanguage
  query <- Query.new elmLanguage querySource
  Query.captures query tree.rootNode

printTree :: Tree -> Effect Unit
printTree = log <<< Node.toString <<< _.rootNode

importAs :: Node -> String
importAs n = case Node.child 1 n, Node.child 2 n >>= Node.child 1 of
  Just m, Just a -> m.text <> "-" <> a.text
  _, _ -> ""

updateSymbols :: FilePath -> Effect Unit
updateSymbols file = do
  elmSource <- liftEffect $ readTextFile UTF8 file
  captures <- getCaptures elmLanguage elmQuerySource elmSource
  talonExecuteUpdate $ commandsFromCaptures captures

documentQuery :: Ref.Ref ServerState -> FilePath -> String -> String -> Position -> HTTPure.ResponseM
documentQuery ref file target type_ position = do
  elmSource <- liftEffect $ readTextFile UTF8 file
  captures <- liftEffect $ captureFilterByType "t" <$> getCaptures elmLanguage (elmQuerySourceMatch type_ target) elmSource
  let
    maybeZipper = toArrayZipperFirst captures
  liftEffect $ Ref.modify_ _ { captureZipper = maybeZipper } ref
  case maybeZipper of
    Nothing -> HTTPure.response 500 "not nice"
    Just zipper -> HTTPure.response 200 $ writeJSON (getFocus (zipperToClosest position zipper)).node

zipperToClosest :: Position -> ArrayZipper Capture -> ArrayZipper Capture
zipperToClosest position zipper = case Zipper.next zipper of
  Just zipper' -> if distance (getFocus zipper') position < distance (getFocus zipper) position then zipperToClosest position zipper' else zipper
  Nothing -> zipper
  where
  distance c p = abs $ c.node.startPosition.row - p.row

documentOpen :: Ref.Ref ServerState -> FilePath -> Effect Unit
documentOpen ref file = do
  programSource <- readTextFile UTF8 file
  tree <- Parser.parse programSource =<< Parser.new elmLanguage
  Ref.modify_ _ { maybeTree = Just tree } ref
  query <- Query.new elmLanguage elmQuerySource
  captures <- Query.captures query tree.rootNode
  talonExecuteUpdate $ commandsFromCaptures captures

cycleResult :: Ref.Ref ServerState -> String -> HTTPure.ResponseM
cycleResult ref direction = do
  serverState <- liftEffect $ Ref.read ref
  case serverState.captureZipper of
    Nothing -> HTTPure.response 400 "No query has been made previously"
    Just zipper -> do
      let
        Tuple move reset = if direction == "backward" then (Tuple Zipper.prev shiftFocusLast) else (Tuple Zipper.next shiftFocusFirst)

        zipper' = move zipper # fromMaybe (reset zipper)
      liftEffect $ Ref.modify_ _ { captureZipper = Just zipper' } ref
      HTTPure.response 200 $ writeJSON (getFocus zipper').node

type ServerState
  = { maybeTree :: Maybe Tree
    , captureZipper :: Maybe (ArrayZipper Capture)
    }

-- | A middleware that logs at the beginning of each request
loggingMiddleware :: (HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM
loggingMiddleware router request = do
  liftEffect do
    log "---------------------"
    logShow request
    log "---------------------"
  router request

server :: HTTPure.ServerM
server = do
  ref <- Ref.new initialState
  HTTPure.serve 8080 (middlewares $ router ref) $ log "Server now up on port 8080"
  where
  initialState = { maybeTree: Nothing, captureZipper: Nothing }

  middlewares = loggingMiddleware

  router :: Ref.Ref ServerState -> HTTPure.Request -> HTTPure.ResponseM
  router ref request = do
    serversState <- liftEffect $ Ref.read ref
    case request of
      { path: [ "select-parent" ], query } -> do
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
      { path: [ "cycle-result" ], query } -> cycleResult ref (query !@ "direction")
      { path: [ "document-query" ], query } -> documentQuery ref (query !@ "file") (query !@ "target") (query !@ "type") (positionFromQuery query # fromMaybe { row: 0, column: 0 })
      { path: [ "document-close" ] } -> do
        liftEffect $ Ref.write initialState ref
        liftEffect $ talonExecuteCommand ClearSymbols
        HTTPure.ok ""
      { path: [ "document-open" ], query } -> do
        liftEffect $ documentOpen ref (query !@ "file")
        HTTPure.ok ""
      _ -> HTTPure.notFound

positionFromQuery :: HTTPure.Query -> Maybe Position
positionFromQuery query = do
  let
    row = Int.fromString (query !@ "line") <#> (_ - 1)

    column = Int.fromString (query !@ "column") <#> (_ - 1)
  case row, column of
    Just r, Just c -> Just { row: r, column: c }
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
  _ -> server *> pure unit
