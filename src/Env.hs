module Env where

import qualified Data.Map as Map
import NativeFunction
import Relude
import Types

defaultEnv :: Env
defaultEnv = nativeFunctions

lookup :: Text -> [Env] -> Maybe Value
lookup name [] = Nothing
lookup name (top : rest) =
  whenNothing (Map.lookup name top) $
    lookup name rest

bind :: Env -> Text -> Value -> Env
bind env name value = Map.insert name value env

bindAll :: Env -> [(Text, Value)] -> Env
bindAll = foldr (\(name, value) acc -> bind acc name value)