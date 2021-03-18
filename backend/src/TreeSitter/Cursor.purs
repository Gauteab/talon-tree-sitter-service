module TreeSitter.Cursor where

import Prelude
import Effect (Effect, whileE)
import TreeSitter (TreeCursor)

foreign import gotoParent :: TreeCursor -> Effect Boolean

foreign import gotoFirstChild :: TreeCursor -> Effect Boolean

foreign import gotoFirstChildForIndex :: Int -> TreeCursor -> Effect Boolean

foreign import gotoNextSibling :: TreeCursor -> Effect Boolean

foreign import reset :: TreeCursor -> Effect Unit

gotoChildAtIndex :: Int -> TreeCursor -> Effect Unit
gotoChildAtIndex index cursor = whileE (gotoFirstChildForIndex index cursor) (pure unit)

gotoParentWithType :: String -> TreeCursor -> Effect Boolean
gotoParentWithType target cursor
  | cursor.nodeType == target = pure true
  | otherwise = do
    success <- gotoParent cursor
    if not success then pure false else gotoParentWithType target cursor
