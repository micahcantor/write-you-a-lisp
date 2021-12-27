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
  Env bindings _ <- get
  case Map.lookup name bindings of
    Just val -> return val
    Nothing -> throwError (UndefinedName name)

bindRec :: Env -> Text -> (Env -> Eval Value) -> Env
bindRec env name getValue =
  env {lookup = lookupRec}
  where
    lookupRec n =
      if n == name
        then get >>= getValue
        else (lookup env) n

bind :: Env -> Text -> Value -> Env
bind env name value = env {bindings = Map.insert name value (bindings env)}

bindAll :: Env -> [(Text, Value)] -> Env
bindAll = foldr (\(name, value) acc -> bind acc name value)