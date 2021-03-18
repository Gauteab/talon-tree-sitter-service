module TreeSitter.Query where

import TreeSitter (Capture, Match, Node, Language, Query)
import Effect (Effect)

foreign import new :: Language -> String -> Effect Query

foreign import captures :: Query -> Node -> Effect (Array Capture)

foreign import matches :: Query -> Node -> Effect (Array Match)
