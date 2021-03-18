module TreeSitter.Parser where

import TreeSitter (Tree, Language, Parser)
import Effect (Effect)

foreign import new :: Language -> Effect Parser

foreign import parse :: String -> Parser -> Effect Tree
