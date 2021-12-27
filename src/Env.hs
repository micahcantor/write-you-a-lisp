module Env where

import Control.Monad.Except
import qualified Data.Map as Map
import NativeFunction
import Relude
import Types

defaultEnv :: Env
defaultEnv =
  Env
    { bindings = nativeFunctions,
      lookup = defaultLookup
    }

defaultLookup :: Text -> Eval Value
defaultLookup name = do
  Env bindings _ <- ask
  case Map.lookup name bindings of
    Just val -> pure val
    Nothing -> throwError (UndefinedName name)

bindRec :: Env -> Text -> (Env -> Eval Value) -> Env
bindRec env name getValue =
  env
    { lookup = \n -> do
        if n == name
          then getValue env
          else defaultLookup n
    }

bind :: Env -> Text -> Value -> Env
bind env name value = bindRec env name (const (pure value))

bindAll :: Env -> [(Text, Value)] -> Env
bindAll = foldr (\(name, value) acc -> bind acc name value)