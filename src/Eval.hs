{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Data.List (head, tail, last)
import qualified Data.Map as Map
import Env
import Relude hiding (head,tail, last)
import Types

runEval :: [Env] -> Eval a -> Either LispError a
runEval envs eval = runIdentity (runExceptT (evalStateT eval envs))

runEvalDefault :: Eval a -> Either LispError a
runEvalDefault = runEval [emptyEnv]

eval :: Value -> Eval Value
eval val = case val of
  Atom name -> getVar name
  String content -> pure (String content)
  Number n -> pure (Number n)
  Boolean b -> pure (Boolean b)
  DottedList xs last -> pure (DottedList xs last)
  List (head : rest) -> case head of
    Atom "if" -> evalIf rest
    Atom "lambda" -> evalLambda rest
    Atom "let" -> evalLet rest
    Atom "begin" -> evalBegin rest
    Atom "quote" -> evalQuote rest
    Atom "define" -> evalDefine rest
    Atom "set!" -> evalSet rest
    f -> apply f rest
  List [] -> pure Nil
  _ -> throwError Default

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
    closure <- getTopEnv
    pure (Function paramNames body closure)
  _ -> throwError (BadSyntax "lambda")

evalLet :: [Value] -> Eval Value
evalLet values = case values of
  List bindings : body -> do
    env <- getTopEnv
    pairs <- forM bindings $ \case
      List [Atom name, expr] -> do
        value <- eval expr
        pure (name, value)
      _ -> throwError (BadSyntax "let")
    withEnv (bindAll emptyEnv pairs) (eval (beginWrap body))
  _ -> throwError (BadSyntax "let")

evalBegin :: [Value] -> Eval Value
evalBegin exprs = do
  values <- mapM eval exprs
  case values of
    [] -> throwError (BadSyntax "begin")
    _ -> pure (last values) -- safe since must be nonempty

evalQuote :: [Value] -> Eval Value
evalQuote values = case values of
  [x] -> pure x
  _ -> throwError (BadSyntax "quote")

evalDefine :: [Value] -> Eval Value
evalDefine values = case values of
  Atom name : body -> do
    value <- eval (beginWrap body)
    modify (\(env : rest) -> (bind name value env) : rest)
    pure Nil
  List (name : args) : body -> do
    let desugared = List [Atom "lambda", List args, beginWrap body]
    evalDefine [name, desugared]
  _ -> throwError (BadSyntax "define")

evalSet :: [Value] -> Eval Value
evalSet values = case values of
  [Atom name, expr] -> do
    value <- eval expr
    _ <- getVar name
    modify (assign name value)
    pure Nil
  _ -> throwError (BadSyntax "set!")

apply :: Value -> [Value] -> Eval Value
apply name args = do
  funValue <- eval name
  argValues <- mapM eval args
  case funValue of
    Function argNames body closure -> do
      let pairs = zip argNames argValues
      withEnv (bindAll closure pairs) (eval body)
    NativeFunction (CallFunc f) -> f argValues
    _ -> throwError (NotFunction funValue)

isTruthy :: Value -> Bool
isTruthy v = case v of
  Boolean False -> False
  _ -> True

beginWrap :: [Value] -> Value
beginWrap values = List (Atom "begin" : values)

getTopEnv :: Eval Env
getTopEnv = do
  envs <- get
  pure (head envs)


withEnv :: Env -> Eval Value -> Eval Value
withEnv env ev = do
  modify ((:) env)
  result <- ev
  modify tail
  pure result

getVar :: Text -> Eval Value
getVar name = do
  envs <- get
  traceShowM envs
  whenNothing (lookup name envs) $ 
    throwError (UndefinedName name)
