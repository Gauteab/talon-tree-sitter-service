module Test.Main where

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Talon as Talon
import Prelude (Unit)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  let
    examples =
      [ Tuple "camelCaseName" "camel case name"
      , Tuple "peekCString" "peek C string"
      , Tuple "snake_case_name" "snake case name"
      , Tuple "foldl1" "foldl 1"
      , Tuple " leading" "leading"
      -- , Tuple "trailing_" "trailing"
      , Tuple "AST" "A S T"
      , Tuple "Test.Unit.Assert" "test unit assert"
      , Tuple "TSNode" "T S node"
      ]
  runTest do
    -- suite "tree stuff" do
    --   test "tree stuff" do
    suite "Normalization" do
      test "normalization" do
        for_ examples \(Tuple input output) -> do
          let
            result = Talon.parseRule input
          case result of
            Left e -> failure ("faled for input " <> show e)
            Right rule -> Assert.equal output (show rule)
