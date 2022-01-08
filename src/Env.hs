module Env where

import qualified Data.Map as Map
import NativeFunction
import Relude
import Types

defaultEnv :: Env
defaultEnv =
  Env
    { bindings = nativeFunctions,
      parent = Nothing
    }

emptyEnv :: Env
emptyEnv = defaultEnv {bindings = Map.empty}

bind :: Text -> Value -> Env -> Env
bind name value env = 
  env {bindings = Map.insert name value (bindings env)}

bindAll :: Env -> [(Text, Value)] -> Env
bindAll = foldr (uncurry bind)

lookup :: Text -> Env -> Maybe Value
lookup name Env{bindings, parent} = 
  whenNothing (Map.lookup name bindings) $ do
    p <- parent
    lookup name p

assign :: Text -> Value -> Env -> Env
assign name value env@Env{bindings, parent} = 
  if Map.member name bindings
    then bind name value env
    else case parent of
      Nothing -> env
      Just p -> env {parent = Just (assign name value p)}