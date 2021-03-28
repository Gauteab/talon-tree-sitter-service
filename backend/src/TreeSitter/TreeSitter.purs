module TreeSitter where

import Prelude
import Data.Ord (Ordering(..))

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

-- instance prdPosition :: Ord Position where
--   compare { row: r1, column: c1 } { row: r2, column: c2 }
--     | r1 > r2 = GT
--     | c1 > c2 = GT
--     | r1 == r2 && c1 == c2 = EQ
--     | otherwise = LT
-- | used for debugging
foreign import json :: forall a. a -> String

foreign import toString :: forall a. a -> String
