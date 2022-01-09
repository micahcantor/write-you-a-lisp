module Types where

import Data.List (unwords)
import qualified Data.Map as Map
import Relude hiding (unwords)
import qualified Text.Show

data Env = Env
  { bindings :: Map Text Value,
    parent :: Maybe Env
  } deriving (Eq)

newtype CallFunc = CallFunc ([Value] -> Eval Value)

instance Eq CallFunc where
  _ == _ = False

data Value
  = Atom Text
  | String Text
  | Number Integer
  | Boolean Bool
  | Function [Text] Value Env -- params, body, and closure
  | Macro [Text] Value Env
  | Primitive CallFunc
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
    Macro _ _ _ -> "<macro>"
    Primitive _ -> "<primitive>"
    List xs -> parenthesized (unwordsList xs)
    DottedList xs last -> parenthesized ((unwordsList xs) <> " . " <> show last)
    Nil -> "nil"

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

parenthesized :: String -> String
parenthesized s = "(" <> s <> ")"

data LispError
  = BadSyntax Text
  | TypeMismatch Text
  | NotCallable Value
  | UndefinedName Text
  | ArityMismatch Text
  | EmptyList Text
  | Default
  deriving (Eq)

instance Show LispError where
  show err =
    "Error: " ++ case err of
      BadSyntax form -> "Bad syntax in form '" ++ toString form ++ "'."
      TypeMismatch f -> "Type mismatch in function '" ++ toString f ++ "'."
      NotCallable v -> "'" ++ show v ++ "' is not callable."
      UndefinedName name -> "Undefined name " ++ toString name
      ArityMismatch f -> "Arity mismatch in function '" ++ toString f ++ "'."
      EmptyList f -> "Empty list in function '" ++ toString f ++ "'."
      Default -> "Default."

type Eval a = StateT Env (ExceptT LispError IO) a