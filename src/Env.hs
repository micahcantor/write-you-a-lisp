module Env where

import Control.Monad.Except (throwError)
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Primitive
import Relude
import Types

defaultEnv :: IO Env
defaultEnv = do
  primitiveRefs <- mapM newIORef primitives
  pure (Env {bindings = primitiveRefs, parent = Nothing})

emptyEnv :: IO Env
emptyEnv = do
  env <- defaultEnv
  pure (env {bindings = Map.empty})

bind :: Text -> Value -> Env -> Eval Env
bind name value env = do
  valueRef <- newIORef value
  pure $ env {bindings = Map.insert name valueRef (bindings env)}

bindAll :: Env -> [(Text, Value)] -> Eval Env
bindAll = foldrM (uncurry bind)

lookup :: Text -> Env -> Eval (Maybe Value)
lookup name Env {bindings, parent} =
  case Map.lookup name bindings of
    Nothing -> case parent of
      Nothing -> pure Nothing
      Just p -> lookup name p
    Just valueRef -> do
      value <- readIORef valueRef
      pure (Just value)

assign :: Text -> Value -> Env -> Eval ()
assign name value env@Env {bindings, parent} =
  case Map.lookup name bindings of
    Nothing -> case parent of
      Nothing -> throwError (UndefinedName name)
      Just p -> assign name value p
    Just valueRef -> writeIORef valueRef value

define :: Text -> Value -> Eval ()
define name value = do
  env <- get
  bound <- bind name value env
  put bound