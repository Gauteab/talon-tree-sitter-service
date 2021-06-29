module Server (server) where

import Prelude
import LangaugeConfig as LanguageConfig
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
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
import Talon (Action(..), OS, commandsFromCaptures)
import Talon as Talon
import TreeSitter (Capture, Position, Tree)
import TreeSitter.Parser as Parser
import TreeSitter.Query as Query
import Util (captureFilterByType, zipperToClosest)

type ServerState
  = { maybeDocument :: Maybe Document
    , captureZipper :: Maybe (ArrayZipper Capture)
    , os :: OS
    }

type Document
  = { tree :: Tree
    , config :: LanguageConfig.LanguageConfig
    }

-- | A request to generate commands for a given program
documentQuery :: Ref ServerState -> String -> String -> Position -> HTTPure.ResponseM
documentQuery ref target type_ position = do
  state <- liftEffect $ Ref.read ref
  case state.maybeDocument of
    Nothing -> HTTPure.response 400 "No document open"
    Just { tree, config } -> do
      query <- liftEffect $ Query.new config.language (LanguageConfig.querySourceMatch config.navigationalQueries type_ target)
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
  let
    maybeConfig = LanguageConfig.fromFileName file
  case maybeConfig of
    Nothing -> log "unknown extension"
    Just config@{ language, symbolQuery } -> do
      tree <- Parser.parse programSource =<< Parser.new language
      let
        document = { tree, config }
      Ref.modify_ _ { maybeDocument = Just document } ref
      query <- Query.new language symbolQuery
      captures <- Query.captures query tree.rootNode
      Talon.executeCommand serverState.os $ UpdateSymbols (commandsFromCaptures captures)

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
  initialState = { maybeDocument: Nothing, captureZipper: Nothing, os: os }

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
        liftEffect $ Talon.executeCommand serversState.os ClearSymbols
        HTTPure.ok ""
      { path: [ "document-open" ], query } -> do
        liftEffect $ documentOpen ref (query !@ "file")
        HTTPure.ok ""
      _ -> HTTPure.notFound

positionFromQuery :: HTTPure.Query -> Maybe Position
positionFromQuery q = case Int.fromString (q !@ "line"), Int.fromString (q !@ "column") of
  Just r, Just c -> Just { row: r - 1, column: c - 1 }
  _, _ -> Nothing
