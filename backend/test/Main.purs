module Test.Main where

import Prelude
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Talon as Talon
import Test.Unit (TestF, failure, success, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import TreeSitter (Language)
import TreeSitter.Parser as Parser
import LangaugeConfig (elmLanguage, pythonLanguage)

testParserCanRun :: forall a. String -> a -> String -> Free TestF Unit
testParserCanRun name language source =
  test name do
    liftEffect $ runParser pythonLanguage source
    success

runParser :: Language -> String -> Effect Unit
runParser langauge source = do
  parser <- Parser.new langauge
  _ <- Parser.parse source parser
  pure unit

normalizationExamples :: Array (Tuple String String)
normalizationExamples =
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

main :: Effect Unit
main = do
  runTest do
    suite "parsers do not crash" do
      testParserCanRun "python" pythonLanguage "x = 5"
      testParserCanRun "elm" elmLanguage "x = 5"
    suite "normalization" do
      test "normalization" do
        for_ normalizationExamples \(Tuple input output) -> case Talon.parseRule input of
          Left e -> failure ("faled for input " <> show e)
          Right rule -> Assert.equal output (show rule)
