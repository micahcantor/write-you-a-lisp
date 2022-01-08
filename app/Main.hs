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
      parseResult <- readFileValues file
      case parseResult of
        Left err -> print err
        Right values -> do
          let topLevel = beginWrap values
          evalResult <- runEvalDefault (eval topLevel)
          case evalResult of
             Left err -> print err
             Right _ -> pass
    _ -> putStrLn "Error: expected 0 or 1 args."
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      whenJust minput $ \input -> do
        case readValue (toText input) of
          Left err -> outputStrLn (show err)
          Right expr -> do
            evalResult <- liftIO $ runEvalDefault (eval expr)
            case evalResult of
              Left err -> outputStrLn (show err)
              Right value -> outputStrLn $ show value
        loop