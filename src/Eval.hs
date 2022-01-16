{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Data.Foldable (foldrM)
import Data.List (last)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Env
import Relude hiding (last)
import Types

runEval :: Env -> Eval a -> IO (Either LispError a)
runEval env eval = runExceptT (evalStateT eval env)

runEvalDefault :: Eval a -> IO (Either LispError a)
runEvalDefault = runEval defaultEnv

eval :: Value -> Eval Value
eval val = case val of
  Atom name -> getVar name
  String content -> pure (String content)
  Number n -> pure (Number n)
  Boolean b -> pure (Boolean b)
  DottedList xs last -> pure (DottedList xs last)
  List (head : rest) -> case head of
    -- Atom "dumpEnv" -> get >>= traceShowM >> pure Nil
    Atom "if" -> evalIf rest
    Atom "lambda" -> evalLambda rest
    Atom "let" -> evalLet rest
    Atom "begin" -> evalBegin rest
    Atom "quote" -> evalQuote rest
    Atom "quasiquote" -> evalQuasiquote rest
    Atom "unquote" -> throwError (BadSyntax "unquote") -- illegal unquote
    Atom "unquote-splicing" -> throwError (BadSyntax "unquote-splicing")
    Atom "define" -> evalDefine rest
    Atom "define-macro" -> evalDefineMacro rest
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
    closure <- get
    paramNames <- getParamNames params
    pure (Function paramNames body closure)
  _ -> throwError (BadSyntax "lambda")

evalLet :: [Value] -> Eval Value
evalLet values = case values of
  List bindings : body -> do
    env <- get
    pairs <- forM bindings $ \case
      List [Atom name, expr] -> do
        value <- eval expr
        pure (name, value)
      _ -> throwError (BadSyntax "let")
    withEnv (bindAll env pairs) (eval (beginWrap body))
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

evalQuasiquote :: [Value] -> Eval Value
evalQuasiquote values = case values of
  [v] -> case v of
    List (Atom "quasiquote" : rest) -> do
      inner <- evalQuasiquote rest
      evalQuasiquote [inner]
    List (Atom "unquote" : rest) ->
      evalUnquote rest
    List [Atom "unquote-splicing", _] ->
      throwError (BadSyntax "unquote-splicing")
    List xs -> do
      values <- foldrM reducer [] xs
      pure (List values)
    _ -> pure v
  _ -> throwError (BadSyntax "quasiquote")
  where
    reducer :: Value -> [Value] -> Eval [Value]
    reducer x acc = case x of
      List [Atom "unquote-splicing", expr] -> case expr of
        List xs -> pure (xs ++ acc)
        _ -> pure (expr : acc)
      List (Atom "unquote-splicing" : _) ->
        throwError (BadSyntax "unquote-splicing")
      List (Atom "unquote" : rest) -> do
        value <- evalUnquote rest
        pure (value : acc)
      List (Atom "quasiquote" : rest) -> do
        inner <- evalQuasiquote rest
        value <- evalQuasiquote [inner]
        pure (value : acc)
      List xs -> do
        values <- foldrM reducer [] xs
        pure (List values : acc)
      _ -> pure (x : acc)

evalUnquote :: [Value] -> Eval Value
evalUnquote values = case values of
  [x] -> eval x
  _ -> throwError (BadSyntax "unquote")

evalDefineMacro :: [Value] -> Eval Value
evalDefineMacro values = case values of
  [List (Atom name : params), body] -> do
    closure <- get
    paramNames <- getParamNames params
    let macro = Macro paramNames body closure
    modify (bind name macro)
    pure Nil
  _ -> throwError (BadSyntax "define-macro")

evalDefine :: [Value] -> Eval Value
evalDefine values = case values of
  [Atom name, body] -> do
    value <- eval body
    modify (bind name value)
    pure Nil
  List (name : params) : body -> do
    let desugared = List [Atom "lambda", List params, beginWrap body]
    evalDefine [name, desugared]
  _ -> throwError (BadSyntax "define")

evalSet :: [Value] -> Eval Value
evalSet values = case values of
  [Atom name, expr] -> do
    env <- get
    value <- eval expr
    case assign name value env of
      Nothing -> throwError (UndefinedName name)
      Just updated -> put updated
    pure Nil
  _ -> throwError (BadSyntax "set!")

apply :: Value -> [Value] -> Eval Value
apply callerExpr argExprs = do
  caller <- eval callerExpr
  case caller of
    Function argNames body closure -> do
      checkArity argNames argExprs
      argValues <- mapM eval argExprs
      let pairs = zip argNames argValues
      withEnv (bindAll closure pairs) (eval body)
    Macro argNames body closure -> do
      checkArity argNames argExprs
      let pairs = zip argNames argExprs -- args are left unevaluated
      expanded <- withEnv (bindAll closure pairs) (eval body)
      eval expanded
    Primitive (CallFunc f) -> do
      argValues <- mapM eval argExprs
      f argValues
    _ -> throwError (NotCallable caller)

isTruthy :: Value -> Bool
isTruthy v = case v of
  Boolean False -> False
  _ -> True

beginWrap :: [Value] -> Value
beginWrap values = List (Atom "begin" : values)

withEnv :: Env -> Eval a -> Eval a
withEnv env computation = do
  put (emptyEnv {parent = Just env})
  result <- computation
  modify (fromJust . parent)
  pure result

getVar :: Text -> Eval Value
getVar name = do
  env <- get
  whenNothing (lookup name env) $
    throwError (UndefinedName name)

checkArity :: [Text] -> [Value] -> Eval ()
checkArity params args =
  if length params == length args
    then pure ()
    else throwError (ArityMismatch "<function>") 

getParamNames :: [Value] -> Eval [Text]
getParamNames params = forM params $ \case
  Atom name -> pure name
  _ -> throwError (BadSyntax "parameter names must all be atoms")

{- getParamNames :: [Value] -> Eval ([Text], Maybe Text)
getParamNames = go ([], Nothing)
  where
    go :: ([Text], Maybe Text) -> [Value] -> Eval ([Text], Maybe Text)
    go (params, varArg) [] = pure (params, varArg)
    go (params, _) [Atom "&", Atom varArg] = pure (params, Just varArg)
    go (params, varArg) (Atom name : rest) = go (name : params, varArg) rest
    go _ _ = throwError (BadSyntax "parameters") -}