module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec (parse)
import qualified Data.Map as Map
import System.Process (callCommand) 

import Parser
import Evaluator
import AST

main :: IO ()
main = do
    args <- getArgs
    case args of

        [] -> putStrLn "Usage: depth <filename>"
        ("depth":filename:_) -> do
            let command = "stack exec DeepX-exe \"" ++ filename ++ "\""
            putStrLn $ "Executing: " ++ command
            callCommand command

        (filename:_) -> do
            putStrLn $ "Running file: " ++ filename
            content <- readFile filename
            let parsed = parse program filename content
            case parsed of
                Left err -> print err
                Right exprs -> do
                    (result, _) <- runEval (evalBlock exprs) Map.empty
                    return ()
