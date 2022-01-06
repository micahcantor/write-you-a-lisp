module Env where

import qualified Data.Map as Map
import NativeFunction
import Relude
import Types

defaultEnv :: Env
defaultEnv = nativeFunctions

emptyEnv :: Env
emptyEnv = Map.empty

lookup :: Text -> [Env] -> Maybe Value
lookup name [] = Nothing
lookup name (top : rest) =
  whenNothing (Map.lookup name top) $
    lookup name rest

assign :: Text -> Value -> [Env] -> [Env]
assign name value = go False []
  where
    go found acc [] = reverse acc
    go found acc (top : rest)
      | found = 
        go found (top : acc) rest
      | Map.member name top = 
        go True ((bind name value top) : acc) rest
      | otherwise = 
        go found (top : acc) rest
    
bind :: Text -> Value -> Env -> Env
bind = Map.insert

bindAll :: Env -> [(Text, Value)] -> Env
bindAll = foldr (uncurry bind)