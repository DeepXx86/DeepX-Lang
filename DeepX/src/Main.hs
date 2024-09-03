module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec (parse)
import qualified Data.Map as Map

import Parser
import Evaluator
import AST

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    content <- readFile filename
    let parsed = parse program filename content
    case parsed of
        Left err -> print err
        Right exprs -> do
            (result, _) <- runEval (evalBlock exprs) Map.empty
            return ()
