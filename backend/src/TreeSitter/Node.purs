module TreeSitter.Node where

import Prelude
import TreeSitter
import Data.Array (find, (!!))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Console (logShow)
import TreeSitter.Cursor (gotoFirstChildForIndex)
import Unsafe.Coerce (unsafeCoerce)

walk :: Node -> TreeCursor
walk node = (unsafeCoerce node :: { walk :: Unit -> TreeCursor }).walk unit

parent :: Node -> Maybe Node
parent node = (unsafeCoerce node :: { parent :: Nullable Node }).parent # Nullable.toMaybe

toString :: Node -> String
toString node = (unsafeCoerce node :: { toString :: Unit -> String }).toString unit

children :: Node -> Array Node
children node = (unsafeCoerce node :: { children :: Array Node }).children

--- Extra
child :: Int -> Node -> Maybe Node
child i n = children n !! i

positionToIndex :: Position -> String -> Int
positionToIndex position text =
  1 + position.column
    + ( String.split (Pattern "\n") text
          # Array.take (position.row + 1)
          # Array.foldMap (Additive <<< String.length)
          # unwrap
      )

-- findByPosition position node
--   | node.startPosition == position = Just node
--   | otherwise =
--     children node
--       # find (\n -> n.startPosition == position)
--       >>= findByPosition position
--       # fromMaybe node
--       # Just
parentWithTypeOn :: (Node -> Maybe Node) -> String -> Node -> Maybe Node
parentWithTypeOn f target node
  | (f node <#> _.type) == Just target = Just node
  | otherwise = parentWithTypeOn f target =<< (parent node)

parentWithType :: String -> Node -> Maybe Node
parentWithType = parentWithTypeOn Just

parentBefore :: String -> Node -> Maybe Node
parentBefore = parentWithTypeOn parent
