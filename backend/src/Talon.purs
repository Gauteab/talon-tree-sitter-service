module Talon where

import Control.MonadZero
import Node.ChildProcess
import Prelude
import Control.Alt ((<|>))
import Data.Array ((:), many)
import Data.Array as Array
import Data.Char.Unicode (isAlpha, isAlphaNum, isLower)
import Data.Char.Unicode as Char
import Data.Either (Either)
import Data.Either as Either
import Data.Filterable (filterMap)
import Data.Foldable (intercalate)
import Data.Lens (_1, _2, over, traversed)
import Data.Map (Map)
import Data.Map as Map
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as String
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile, realpath)
import Node.Process (exit)
import Node.Stream (onDataString)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (eof, satisfy)
import Text.Parsing.Parser.Token (digit)
import TreeSitter (Capture, Position)
import Util (groupOn, keyValuePair, keyValuePair', surround, (~))

foreign import homedir :: Effect String

many1 :: forall a. Parser String a -> Parser String (Array a)
many1 p = (:) <$> p <*> many p

parseRule :: String -> Either ParseError Rule
parseRule s = runParser s $ Rule <$> many rulePart <* garbage <* eof
  where
  garbage = many $ satisfy (not isAlphaNum)

  rulePart = garbage *> (number <|> wordOrLetter)

  wordOrLetter = do
    first <- satisfy isAlpha
    rest <- many (satisfy isLower)
    if rest == [] then
      pure $ Letter $ Char.toUpper first
    else
      pure $ Word $ String.fromCharArray $ Char.toLower first : rest

  number = Number <$> many1 digit

data Action
  = UpdateSymbols CommandSet
  | ClearSymbols
  | Navigate Int Int
  | SelectRange Position Position

instance showAction :: Show Action where
  show (UpdateSymbols cs) = makeActionString "update_symbols" [ show cs ]
  show ClearSymbols = makeActionString "clear_symbols" []
  show (Navigate row column) = makeActionString "vim_go_to_position" [ show row, show column ]
  show (SelectRange p1 p2) = makeActionString "vim_select_range" (show <$> [ p1.row, p1.column, p2.row, p2.column ])

makeActionString :: String -> Array String -> String
makeActionString name arguments = "actions.user." <> name <> "(" <> intercalate "," arguments <> ")"

data OS
  = Windows
  | Unix

-- talonExecuteUpdate :: CommandSet -> Effect Unit
-- talonExecuteUpdate = UpdateSymbols >>> talonExecuteCommand
talonExecuteCommand :: OS -> Action -> Effect Unit
talonExecuteCommand Windows = talonExecuteCommandWindows

talonExecuteCommand Unix = talonExecuteCommandUnix

-- | Slower solution to IPC which does not require
-- | access to the repl, which currently does not work on WSL
-- | Simply writes the command to a file that is watched by talon
talonExecuteCommandWindows :: Action -> Effect Unit
talonExecuteCommandWindows statement = do
  home <- homedir
  let
    path = home <> "/.talon-tss-ipc/cmd"
  writeTextFile UTF8 path (show statement)

-- | Since a command to the talon repl
-- | TODO: make better use of the child process API. this is hacky
talonExecuteCommandUnix :: Action -> Effect Unit
talonExecuteCommandUnix statement = do
  let
    cmd = "echo \"" <> show statement <> "\" | ~/.talon/.venv/bin/repl"
  log cmd
  process <- spawn "sh" [ "-c", cmd ] defaultSpawnOptions
  onError process logShow
  onDataString (stdout process) UTF8 \s ->
    when (Array.elem "Traceback (most recent call last):" $ lines s) do
      log s
      exit 1

newtype Rule
  = Rule (Array RulePart)

derive instance ruleNewtype :: Newtype Rule _

data RulePart
  = Word String
  | Letter Char
  | Number (Array Char)

instance showRulePart :: Show RulePart where
  show (Word s) = s
  show (Letter c) = String.fromCharArray [ c ]
  show (Number cs) = String.fromCharArray cs

instance ruleShow :: Show Rule where
  show = unwrap >>> map show >>> intercalate " "

type Command
  = Tuple Rule String

-- | Data structure containing groups of rules
newtype CommandSet
  = CommandSet (Array (Tuple String (Array Command)))

overrides :: Map String String
overrides = Map.fromFoldable [ "init" ~ "in it" ]

commandsFromCaptures :: Array Capture -> CommandSet
commandsFromCaptures =
  groupCaptures
    >>> over (traversed <<< _2 <<< traversed) captureToCommand
    >>> over (traversed <<< _2) (filterMap Either.hush)
    >>> CommandSet
  where
  groupCaptures :: Array Capture -> Array (Tuple String (Array Capture))
  groupCaptures = groupOn _.name >>> Map.toUnfoldable

  captureToCommand :: Capture -> Either ParseError Command
  captureToCommand capture = Tuple <$> parseRule text' <*> pure text
    where
    text = capture.node.text

    text' = Map.lookup text overrides # Maybe.fromMaybe text

-- TODO: Rewrite
instance commandSetShow :: Show CommandSet where
  show commandSet = toDictionary $ map f (g commandSet)
    where
    toDictionary :: Array String -> String
    toDictionary = surround "{" "}" <<< Array.intercalate ","

    g :: CommandSet -> Array (Tuple String (Array (Tuple String String)))
    g (CommandSet a) = over (traversed <<< _2 <<< traversed <<< _1) show a

    f (Tuple name commands) = keyValuePair name (toDictionary $ map (uncurry keyValuePair') commands)

-- f (Tuple name commands) = keyValuePair ("user." <> name) (toDictionary $ map (uncurry keyValuePair') commands)
