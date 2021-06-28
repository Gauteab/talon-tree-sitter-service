module LangaugeConfig where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import TreeSitter (Language)

foreign import elmLanguage :: Language

foreign import pythonLanguage :: Language

type LanguageConfig
  = { language :: Language
    , symbolQuery :: String
    -- , navigationalQuery :: Maybe String
    }

queryMapping :: Map String String
queryMapping =
  Map.fromFoldable
    [ Tuple "type" "[ (type_declaration (upper_case_identifier) @i) (type_alias_declaration (upper_case_identifier) @i) ] @t"
    , Tuple "identifier" "[ (lower_case_identifier) @i (upper_case_identifier) @i ] @t"
    , Tuple "function" "(value_declaration (function_declaration_left (lower_case_identifier) @i)) @t"
    , Tuple "import" "(import_clause) @t"
    , Tuple "string" "(string_constant_expr) @t"
    , Tuple "number" "(number_constant_expr) @t"
    ]

createMatchQuery :: String -> String -> Maybe String
createMatchQuery name match = do
  pattern <- Map.lookup name queryMapping
  Just $ if match == "" then pattern else "(\n" <> pattern <> " " <> "\n(#eq? @i \"" <> match <> "\")\n)"

elmQuerySourceMatch :: String -> String -> String
elmQuerySourceMatch name match = fromMaybe "" $ createMatchQuery name match

pythonQuerySource :: String
pythonQuerySource =
  """
(identifier) @identifier 
(class_definition (identifier) @type)
(function_definition (identifier) @type)
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
elmQuerySource :: String
elmQuerySource =
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

elm :: LanguageConfig
elm = { language: elmLanguage, symbolQuery: elmQuerySource }

python :: LanguageConfig
python = { language: pythonLanguage, symbolQuery: pythonQuerySource }

extensionMap :: Map String LanguageConfig
extensionMap =
  Map.fromFoldable
    [ Tuple "py" python
    , Tuple "elm" elm
    ]
