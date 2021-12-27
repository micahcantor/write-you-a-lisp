{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Data.List (last)
import qualified Data.Map as Map
import NativeFunction
import Relude hiding (last)
import Types
import Env

runEval :: Env -> Eval a -> Either LispError a
runEval env eval = runIdentity (runExceptT (runReaderT eval env))

runEvalDefault :: Eval a -> Either LispError a
runEvalDefault = runEval defaultEnv

eval :: Value -> Eval Value
eval val = do
  traceShowM val
  case val of
    Atom name -> getVar name
    String content -> pure (String content)
    Number n -> pure (Number n)
    Boolean b -> pure (Boolean b)
    Pair car cdr -> do
      carVal <- eval car
      cdrVal <- eval cdr
      pure (Pair carVal cdrVal)
    List (head : rest) -> case head of
      Atom "if" -> evalIf rest
      Atom "lambda" -> evalLambda rest
      Atom "let" -> evalLet rest
      Atom "begin" -> evalBegin rest
      Atom "define" -> do
        evalDefine rest
      _ -> evalApplication head rest
    List [] -> pure (List [])
    _ -> throwError Default -- this is because not distinguishing between expressions and values

withEnv :: Env -> Eval Value -> Eval Value 
withEnv env = local (const env)

getVar :: Text -> Eval Value
getVar name = do
  Env _ lookup <- ask
  lookup name

evalIf :: [Value] -> Eval Value
evalIf values = case values of
  [predicate, consequent, alternate] -> do
    predValue <- eval predicate
    if isTruthy predValue
      then eval consequent
      else eval alternate
  _ -> throwError (BadSyntax "if")

evalLambda :: [Value] -> Eval Value
evalLambda values = case values of
  [List params, body] -> do
    paramNames <- forM params $ \case
      Atom name -> pure name
      _ -> throwError (BadSyntax "lambda")
    env <- ask
    pure $ Function paramNames body env
  _ -> throwError (BadSyntax "lambda")

evalLet :: [Value] -> Eval Value
evalLet values = case values of
  [List bindings, body] -> do
    env <- ask
    pairs <- forM bindings $ \case
      List [Atom name, value] -> do
        value' <- eval value
        pure (name, value')
      _ -> throwError (BadSyntax "let")
    withEnv (bindAll env pairs) (pure body)
  _ -> throwError (BadSyntax "let")

evalBegin :: [Value] -> Eval Value
evalBegin values = do
  values' <- mapM eval values
  case values' of
    [] -> throwError (BadSyntax "begin")
    _ -> pure (last values') -- safe since must be nonempty

evalDefine :: [Value]  -> Eval Value
evalDefine values = case values of
  [Atom name, body] -> do
    env <- ask
    let env' = bindRec env name (`withEnv` (pure body))
    traceShowM (List rest)
    withEnv env' (pure (List rest))
  _ -> throwError (BadSyntax "define")

evalApplication :: Value -> [Value] -> Eval Value
evalApplication fun args = do
  funValue <- eval fun
  argValues <- mapM eval args
  case funValue of
    Function argNames body closure -> do
      let pairs = zip argNames argValues
      withEnv (bindAll closure pairs) (pure body)
    NativeFunction f -> f argValues
    _ -> throwError (NotFunction funValue)

isTruthy :: Value -> Bool
isTruthy v = case v of
  Boolean False -> False
  _ -> True

makeTopLevel :: [Value] -> Value
makeTopLevel values = List (Atom "begin" : values)