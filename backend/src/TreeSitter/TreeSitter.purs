module TreeSitter where

import Prelude

foreign import data Language :: Type

foreign import elmLanguage :: Language

foreign import data Parser :: Type

type Tree
  = { rootNode :: Node }

type Node
  = { text :: String
    , startPosition :: Position
    , endPosition :: Position
    , isNamed :: Boolean
    , type :: String
    }

type TreeCursor
  = { startIndex :: Int
    , endIndex :: Int
    , startPosition :: Position
    , endPosition :: Position
    , nodeType :: String
    , nodeIsNamed :: Boolean
    , currentNode :: Node
    }

foreign import data Query :: Type

type Match
  = { pattern :: Int
    , captures :: Array Capture
    }

type Capture
  = { name :: String, node :: Node }

--- Data Structures
type Position
  = { row :: Int, column :: Int }

-- | used for debugging
foreign import json :: forall a. a -> String

foreign import toString :: forall a. a -> String
