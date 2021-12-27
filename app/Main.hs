module Main where

import Eval
import Parser
import Relude
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> runInputT defaultSettings loop
    [file] -> do
      result <- readFileValues file
      case result of
        Left err -> print err
        Right values ->
          let topLevel = makeTopLevel values
           in case runEvalDefault (eval topLevel) of
             Left err -> print err
             Right result -> print result
    _ -> putStrLn "0 or 1 args"
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      whenJust minput $ \input -> do
        case readValue (toText input) of
          Left err -> outputStrLn (show err)
          Right expr -> case runEvalDefault (eval expr) of
            Left err -> outputStrLn (show err)
            Right value -> outputStrLn $ show value
        loop