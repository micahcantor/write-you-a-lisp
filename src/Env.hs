module Env where

import qualified Data.Map as Map
import Primitive
import Relude
import Types

defaultEnv :: Env
defaultEnv =
  Env
    { bindings = primitives,
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

assign :: Text -> Value -> Env -> Maybe Env
assign name value env@Env{bindings, parent} = 
  if Map.member name bindings
    then Just (bind name value env)
    else do
      p <- parent
      Just (env {parent = assign name value p})