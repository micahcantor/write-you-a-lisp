{-# LANGUAGE LambdaCase #-}

module NativeFunction where

import Control.Monad.Except
import qualified Data.Map as Map
import Relude
import Types
import Data.List (foldl1')

nativeFunctions :: Map Text Value
nativeFunctions =
  NativeFunction . CallFunc
    <$> Map.fromList
      [ ("+", add),
        ("-", sub),
        ("*", times),
        ("=", eq),
        ("car", car),
        ("cdr", cdr),
        ("cons", cons),
        ("null?", isNull)
      ]

getNums :: Text -> [Value] -> Eval [Integer]
getNums name xs = do
  nums <- forM xs $ \case
    Number n -> pure n
    _ -> throwError (TypeMismatch name)
  pure nums

add :: [Value] -> Eval Value
add [] = pure (Number 0)
add xs = do
  nums <- getNums "+" xs
  pure (Number (sum nums))

sub :: [Value] -> Eval Value
sub [] = throwError (ArityMismatch "-")
sub [x] = case x of
  Number n -> pure (Number (negate n))
  _ -> throwError (TypeMismatch "-")
sub xs = do
  nums <- getNums "-" xs
  pure (Number (foldl1' (-) nums))

times :: [Value] -> Eval Value 
times [] = pure (Number 1)
times xs = do
  nums <- getNums "*" xs
  pure (Number (product nums))

eq :: [Value] -> Eval Value
eq [x, y] = pure (Boolean (x == y))
eq _ = throwError (ArityMismatch "=")

cons :: [Value] -> Eval Value
cons [head, tail] = case tail of
  Nil -> pure (List [head])
  List xs -> pure (List (head : xs))
  _ -> throwError (TypeMismatch "cons")
cons _ = throwError (ArityMismatch "cons")

car :: [Value] -> Eval Value
car [x] = case x of
  Nil -> throwError (EmptyList "car")
  List (x : _) -> pure x
  _ -> throwError (TypeMismatch "car")
car _ = throwError (ArityMismatch "car")

cdr :: [Value] -> Eval Value
cdr [x] = case x of
  Nil -> throwError (EmptyList "cdr")
  List (_ : xs) -> pure (List xs)
  _ -> throwError (TypeMismatch "cdr")
cdr _ = throwError (ArityMismatch "cdr")

isNull :: [Value] -> Eval Value
isNull [x] = case x of
  Nil -> pure (Boolean True)
  List [] -> pure (Boolean True)
  _ -> pure (Boolean False)
isNull _ = throwError (ArityMismatch "null?")