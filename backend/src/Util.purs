module Util where

import Prelude
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Data.Zipper.ArrayZipper (ArrayZipper, getFocus)
import Data.Zipper.ArrayZipper as Zipper
import TreeSitter (Position, Capture, Node)
import TreeSitter.Node as Node

infixr 5 Tuple as ~

surround :: forall a. Semigroup a => a -> a -> a -> a
surround before after content = before <> content <> after

surround' :: forall a. Semigroup a => a -> a -> a
surround' around = surround around around

keyValuePair :: String -> String -> String
keyValuePair k v = "'" <> k <> "':" <> v <> ""

keyValuePair' :: String -> String -> String
keyValuePair' k v = "'" <> k <> "':'" <> v <> "'"

groupOn :: forall a k. Ord k => (a -> k) -> Array a -> Map k (Array a)
groupOn f = Array.foldr (\a b -> Map.unionWith (<>) b $ Map.singleton (f a) [ a ]) Map.empty

zipperToClosest :: Position -> ArrayZipper Capture -> ArrayZipper Capture
zipperToClosest position zipper = case Zipper.next zipper of
  Just zipper' -> if distance (getFocus zipper') position < distance (getFocus zipper) position then zipperToClosest position zipper' else zipper
  Nothing -> zipper
  where
  distance c p = abs $ toNumber (c.node.startPosition.row - p.row) + 0.1 * (toNumber $ c.node.startPosition.column - p.column)

captureFilterByType :: String -> Array Capture -> Array Capture
captureFilterByType typeName = Array.filter $ (_ == typeName) <<< _.name

importAs :: Node -> String
importAs n = case Node.child 1 n, Node.child 2 n >>= Node.child 1 of
  Just m, Just a -> m.text <> "-" <> a.text
  _, _ -> ""
