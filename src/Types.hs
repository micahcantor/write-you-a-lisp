module Types where

import Data.List (unwords)
import Relude hiding (unwords)
import qualified Text.Show
import Util
import qualified Data.Map as Map

data Env = Env
  { bindings :: Map Text Value,
    lookup :: Text -> Eval Value
  }

instance Show Env where
  show (Env bindings _) = show bindings

data Value
  = Atom Text
  | String Text
  | Number Integer
  | Boolean Bool
  | Function [Text] Value Env -- params, body, and closure
  | NativeFunction ([Value] -> Eval Value)
  | List [Value]
  | Pair Value Value
  | Nil

instance Show Value where
  show val = case val of
    Number n -> show n
    Boolean True -> "#t"
    Boolean False -> "#f"
    Atom text -> "\'" <> toString text
    String text -> "\"" <> toString text <> "\""
    Function _ _ _ -> "<function>"
    NativeFunction _ -> "<native-function>"
    List xs -> parenthesized (unwordsList xs)
    Pair a b -> parenthesized (show a <> " . " <> show b)
    Nil -> "nil"

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

data LispError
  = BadSyntax Text
  | TypeMismatch Text
  | NotFunction Value
  | UndefinedName Text
  | ArityMismatch Text
  | EmptyList Text
  | Default
  deriving (Show)

type Eval a = ReaderT Env (ExceptT LispError Identity) a