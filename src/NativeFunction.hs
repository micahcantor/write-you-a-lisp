{-# LANGUAGE LambdaCase #-}

module NativeFunction where

import Control.Monad.Except
import Data.Foldable (foldl1)
import qualified Data.Map as Map
import Relude
import Types

nativeFunctions :: Map Text Value
nativeFunctions =
  NativeFunction
    <$> Map.fromList
      [ ("+", add),
        ("-", sub),
        ("eq", eq),
        ("car", car),
        ("cdr", cdr),
        ("cons", cons),
        ("null?", isNull)
      ]

getNums :: Text -> [Value] -> Eval [Integer]
getNums name xs = do
  nums <- forM xs $ \case
    Number n -> return n
    _ -> throwError (TypeMismatch name)
  return nums

add :: [Value] -> Eval Value
add [] = return (Number 0)
add xs = do
  nums <- getNums "+" xs
  return (Number (sum nums))

sub :: [Value] -> Eval Value
sub [] = throwError (ArityMismatch "-")
sub [x] = case x of
  Number n -> return (Number (negate n))
  _ -> throwError (TypeMismatch "-")
sub xs = do
  nums <- getNums "-" xs
  return (Number (foldl1 (-) nums))

eq :: [Value] -> Eval Value
eq [x, y] = case (x, y) of
  (Number n, Number m) -> return (Boolean (n == m))
  _ -> throwError (TypeMismatch "==")
eq _ = throwError (ArityMismatch "==")

cons :: [Value] -> Eval Value
cons [head, tail] = case tail of
  Nil -> return (List [head])
  List xs -> return (List (head : xs))
  _ -> throwError (TypeMismatch "cons")
cons _ = throwError (ArityMismatch "cons")

car :: [Value] -> Eval Value
car [x] = case x of
  Nil -> throwError (EmptyList "car")
  List (x : _) -> return x
  _ -> throwError (TypeMismatch "car")
car _ = throwError (ArityMismatch "car")

cdr :: [Value] -> Eval Value
cdr [x] = case x of
  Nil -> throwError (EmptyList "cdr")
  List (_ : xs) -> return (List xs)
  _ -> throwError (TypeMismatch "cdr")
cdr _ = throwError (ArityMismatch "cdr")

isNull :: [Value] -> Eval Value
isNull [x] = case x of
  Nil -> return (Boolean True)
  List [] -> return (Boolean True)
  _ -> return (Boolean False)
isNull _ = throwError (ArityMismatch "null?")