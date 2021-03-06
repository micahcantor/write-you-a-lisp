{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Data.Foldable (foldrM)
import Data.List (last)
import qualified Data.Map as Map
import Env
import Relude hiding (last)
import Types

runEval :: Env -> Eval a -> IO (Either LispError a)
runEval env eval = runExceptT (evalStateT eval env)

runEvalDefault :: Eval a -> IO (Either LispError a)
runEvalDefault computation = do
  env <- defaultEnv
  runEval env computation

eval :: Value -> Eval Value
eval val = case val of
  Atom name -> getVar name
  String content -> pure (String content)
  Number n -> pure (Number n)
  Boolean b -> pure (Boolean b)
  List (head : rest) -> case head of
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
    (paramNames, varArg) <- getParamNames params
    pure (Function paramNames body closure varArg)
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
    bound <- bindAll env pairs
    withEnv bound (eval (beginWrap body))
  _ -> throwError (BadSyntax "let")

evalBegin :: [Value] -> Eval Value
evalBegin exprs = do
  values <- mapM eval exprs
  case values of
    [] -> throwError (BadSyntax "begin")
    _ -> pure (last values) -- safe since must be nonempty

evalDefine :: [Value] -> Eval Value
evalDefine values = case values of
  [Atom name, body] -> do
    value <- eval body
    define name value
    fixClosure name value
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
    assign name value env
    pure Nil
  _ -> throwError (BadSyntax "set!")

apply :: Value -> [Value] -> Eval Value
apply callerExpr argExprs = do
  caller <- eval callerExpr
  case caller of
    Function paramNames body closure varArg -> do
      checkArity paramNames (take (length paramNames) argExprs)
      argValues <- mapM eval argExprs
      let pairs = makeParamArgumentPairs paramNames argValues varArg
      env <- bindAll closure pairs
      withEnv env (eval body)
    Macro paramNames body closure varArg -> do
      checkArity paramNames (take (length paramNames) argExprs)
      let pairs = makeParamArgumentPairs paramNames argExprs varArg
      env <- bindAll closure pairs
      expanded <- withEnv env (eval body)
      eval expanded
    Primitive (CallFunc f) -> do
      argValues <- mapM eval argExprs
      f argValues
    _ -> throwError (NotCallable caller)

makeParamArgumentPairs :: [Text] -> [Value] -> Maybe Text -> [(Text, Value)]
makeParamArgumentPairs paramNames args varArg =
  case varArg of
    Just varArgName -> 
      let (singles, vars) = splitAt (length paramNames) args
          varArgValue = List vars
       in (zip paramNames args) ++ [(varArgName, varArgValue)] 
    Nothing -> zip paramNames args

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
      values <- foldrM evalQuasiquoteList [] xs
      pure (List values)
    _ -> pure v
  _ -> throwError (BadSyntax "quasiquote")
  where
    evalQuasiquoteList :: Value -> [Value] -> Eval [Value]
    evalQuasiquoteList x acc = case x of
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
        values <- foldrM evalQuasiquoteList [] xs
        pure (List values : acc)
      _ -> pure (x : acc)

evalUnquote :: [Value] -> Eval Value
evalUnquote values = case values of
  [x] -> eval x
  _ -> throwError (BadSyntax "unquote")

evalDefineMacro :: [Value] -> Eval Value
evalDefineMacro values = case values of
  [List (Atom name : params), body] -> do
    env <- get
    (paramNames, varArg) <- getParamNames params
    let macro = Macro paramNames body env varArg
    define name macro
    pure Nil
  _ -> throwError (BadSyntax "define-macro")

isTruthy :: Value -> Bool
isTruthy v = case v of
  Boolean False -> False
  _ -> True

beginWrap :: [Value] -> Value
beginWrap values = List (Atom "begin" : values)

-- run a computation in a given environment
withEnv :: Env -> Eval a -> Eval a
withEnv env computation = do
  old <- get
  put env
  result <- computation
  put old
  pure result

getVar :: Text -> Eval Value
getVar name = do
  env <- get
  value <- lookup name env
  whenNothing value $
    throwError (UndefinedName name)

checkArity :: [Text] -> [Value] -> Eval ()
checkArity params args =
  if length params == length args
    then pure ()
    else throwError (ArityMismatch "<function>")

-- stuff the definition of a function or macro into its closure
-- so that it can be referenced recursively.
fixClosure :: Text -> Value -> Eval ()
fixClosure name value = do
  env <- get
  case value of
    Function args body closure varArg -> do
      updatedClosure <- bind name value closure
      let updatedFn = Function args body updatedClosure varArg
      assign name updatedFn env
      assign name updatedFn updatedClosure
    Macro args body closure varArg -> do
      updatedClosure <- bind name value closure
      let updatedMacro = Macro args body updatedClosure varArg
      assign name updatedMacro env
      assign name updatedMacro updatedClosure
    _ -> pure ()

getParamNames :: [Value] -> Eval ([Text], Maybe Text)
getParamNames [] = pure ([], Nothing)
getParamNames params = do
  atoms <- getAtoms params
  if "&" `elem` atoms
    then do
      let singles = takeWhile (/= "&") atoms
      let varArg = last atoms
      if varArg == "&"
        then throwError (BadSyntax "missing variable argument after ampersand")
        else pure (singles, Just varArg)
    else pure (atoms, Nothing)
  where
    getAtoms :: [Value] -> Eval [Text]
    getAtoms params = forM params $ \case
      Atom name -> pure name
      _ -> throwError (BadSyntax "parameter names must all be atoms")