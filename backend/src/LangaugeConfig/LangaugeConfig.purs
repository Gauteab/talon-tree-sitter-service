module LangaugeConfig
  ( querySourceMatch
  , elm
  , python
  , fromFileName
  , LanguageConfig
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.NonEmpty as NonEmptyString
import Data.Tuple (Tuple(..))
import Pathy (Name(..), extension)
import TreeSitter (Language)

foreign import elmLanguage :: Language

foreign import pythonLanguage :: Language

type LanguageConfig
  = { language :: Language
    , symbolQuery :: String
    , navigationalQueries :: Map String String
    }

createMatchQuery :: Map String String -> String -> String -> Maybe String
createMatchQuery queryMapping name match = do
  pattern <- Map.lookup name queryMapping
  Just $ if match == "" then pattern else "(\n" <> pattern <> " " <> "\n(#eq? @i \"" <> match <> "\")\n)"

querySourceMatch :: Map String String -> String -> String -> String
querySourceMatch queryMapping name match = fromMaybe "" $ createMatchQuery queryMapping name match

elm :: LanguageConfig
elm =
  { language: elmLanguage
  , symbolQuery:
      """
(lower_case_identifier) @identifier 
(upper_case_identifier) @identifier
(function_declaration_left (lower_case_identifier) @function)
(exposed_value) @function
(lower_pattern) @function
(exposed_type (upper_case_identifier) @type)
(type_declaration (upper_case_identifier) @type)
(type_alias_declaration (upper_case_identifier) @type)
"""
  -- (union_variant (upper_case_identifier) @constructor)
  -- (record_pattern (_ (lower_case_identifier) @record-field))
  -- (field_access_expr (lower_case_identifier) @record-field)
  -- (field_type name: (lower_case_identifier) @record-field)
  -- (field name: (lower_case_identifier) @record-field)
  -- (import_clause (upper_case_qid (upper_case_identifier) @module))
  -- (import_clause (as_clause (upper_case_identifier) @module-alias))
  -- (import_clause (as_clause)) @import-with-alias
  --   """
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
  , navigationalQueries:
      Map.fromFoldable
        [ Tuple "type" "[ (type_declaration (upper_case_identifier) @i) (type_alias_declaration (upper_case_identifier) @i) ] @t"
        , Tuple "identifier" "[ (lower_case_identifier) @i (upper_case_identifier) @i ] @t"
        , Tuple "function" "(value_declaration (function_declaration_left (lower_case_identifier) @i)) @t"
        , Tuple "import" "(import_clause) @t"
        , Tuple "string" "(string_constant_expr) @t"
        , Tuple "number" "(number_constant_expr) @t"
        ]
  }

python :: LanguageConfig
python =
  { language: pythonLanguage
  , symbolQuery:
      """
(identifier) @identifier 
(class_definition (identifier) @type)
(function_definition (identifier) @function)
(import_statement name: (dotted_name) @module)
(aliased_import (dotted_name) @module alias: (identifier) @module)
(import_from_statement module_name: (dotted_name) @module (dotted_name) @value)
"""
  , navigationalQueries: Map.empty
  }

fromFileExtension :: String -> Maybe LanguageConfig
fromFileExtension = case _ of
  "py" -> Just python
  "elm" -> Just elm
  _ -> Nothing

fromFileName :: String -> Maybe LanguageConfig
fromFileName file = do
  nes <- Name <$> NonEmptyString.fromString file
  e <- extension nes
  fromFileExtension (NonEmptyString.toString e)
