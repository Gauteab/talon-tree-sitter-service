module Util where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Tuple (Tuple(..))

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
