{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Data.List (last)
import qualified Data.Map as Map
import Env
import NativeFunction
import Relude hiding (last)
import Types

runEval :: Env -> Eval a -> Either LispError a
runEval env eval = runIdentity (runExceptT (evalStateT eval env))

runEvalDefault :: Eval a -> Either LispError a
runEvalDefault = runEval defaultEnv

eval :: Value -> Eval Value
eval val = case val of
  Atom name -> getVar name
  String content -> return (String content)
  Number n -> return (Number n)
  Boolean b -> return (Boolean b)
  Pair car cdr -> do
    carVal <- eval car
    cdrVal <- eval cdr
    return (Pair carVal cdrVal)
  List (head : rest) -> case head of
    Atom "if" -> evalIf rest
    Atom "lambda" -> evalLambda rest
    Atom "let" -> evalLet rest
    Atom "begin" -> evalBegin rest
    Atom "quote" -> evalQuote rest
    Atom "define" -> evalDefine rest
    _ -> apply head rest
  List [] -> return Nil
  _ -> throwError Default -- this is because not distinguishing between expressions and values

withEnv :: Env -> Value -> Eval Value
withEnv env val = do
  put env
  eval val

getVar :: Text -> Eval Value
getVar name = do
  Env bindings lookup <- get
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
      Atom name -> return name
      _ -> throwError (BadSyntax "lambda")
    closure <- get
    return $ Function paramNames body closure
  _ -> throwError (BadSyntax "lambda")

evalLet :: [Value] -> Eval Value
evalLet values = case values of
  [List bindings, body] -> do
    env <- get
    pairs <- forM bindings $ \case
      List [Atom name, value] -> do
        value' <- eval value
        return (name, value')
      _ -> throwError (BadSyntax "let")
    withEnv (bindAll env pairs) body
  _ -> throwError (BadSyntax "let")

evalBegin :: [Value] -> Eval Value
evalBegin values = do
  values' <- mapM eval values
  case values' of
    [] -> throwError (BadSyntax "begin")
    _ -> return (last values') -- safe since must be nonempty

evalQuote :: [Value] -> Eval Value
evalQuote values = case values of
  [x] -> return x
  _ -> throwError (BadSyntax "quote")

evalDefine :: [Value] -> Eval Value
evalDefine values = case values of
  [Atom name, body] -> do 
    modify (\env -> bindRec env name (`withEnv` body))
    return Nil
  _ -> throwError (BadSyntax "define")

apply :: Value -> [Value] -> Eval Value
apply fun args = do
  funValue <- eval fun
  argValues <- mapM eval args
  case funValue of
    Function argNames body closure -> do
      let pairs = zip argNames argValues
      withEnv (bindAll closure pairs) body
    NativeFunction f -> f argValues
    _ -> throwError (NotFunction funValue)

isTruthy :: Value -> Bool
isTruthy v = case v of
  Boolean False -> False
  _ -> True

makeTopLevel :: [Value] -> Value
makeTopLevel values = List (Atom "begin" : values)