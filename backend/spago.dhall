{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "talon-tree-sitter-service"
, dependencies =
  [ "arrays"
  , "arrays-zipper"
  , "console"
  , "debug"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "httpure"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "nullable"
  , "ordered-collections"
  , "parsing"
  , "pathy"
  , "profunctor-lenses"
  , "psci-support"
  , "refs"
  , "simple-json"
  , "string-parsers"
  , "stringutils"
  , "test-unit"
  , "undefined"
  , "unicode"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
