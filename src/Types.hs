module Types where

import Data.List (unwords)
import Relude hiding (unwords)
import qualified Text.Show
import Util
import qualified Data.Map as Map

type Env = Map Text Value

newtype CallFunc = CallFunc ([Value] -> Eval Value)

instance Eq CallFunc where
  _ == _ = False

data Value
  = Atom Text
  | String Text
  | Number Integer
  | Boolean Bool
  | Function [Text] Value Env -- params, body, and closure
  | NativeFunction CallFunc
  | List [Value]
  | DottedList [Value] Value
  | Nil
  deriving (Eq)

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
    DottedList xs last -> ""
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
  deriving (Show, Eq)

type Eval a = StateT [Env] (ExceptT LispError Identity) a