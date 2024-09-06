module Evaluator where

import Control.Monad.State
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forever, foldM)
import System.IO (hFlush, stdout, writeFile, withFile, IOMode(WriteMode))
import Text.Read (readMaybe)
import System.Directory (listDirectory, removeFile, getCurrentDirectory, renameFile, setCurrentDirectory, doesDirectoryExist)
import System.Process (callCommand)
import System.Info (os)
import Parser
import Text.Parsec
import AST
    ( Value(..),
      Expr(SaveAs, Depth, Literal, Var, Add, Sub, Mul, Div, LessThan,
           GreaterThan, Equals, Assign, FuncCall, FuncDef, Write, Import,
           While, For, Wait, If, GetLine, DeskList, DeskDel, DeskCd,
           DeskRename, DeskMake) )

type Env = Map.Map String Value
type Eval a = StateT Env IO a

stringToInt :: String -> Maybe Int
stringToInt = readMaybe

saveFileToLocation :: FilePath -> String -> IO ()
saveFileToLocation location content = do
    dirExists <- doesDirectoryExist location
    if dirExists
        then writeFile (location ++ "\\newfile.txt") content
        else do
            currentDir <- getCurrentDirectory
            writeFile (currentDir ++ "\\newfile.txt") content

eval :: Expr -> Eval Value

eval (Depth filepath) = do
    content <- liftIO $ readFile filepath
    case parse program filepath content of
        Left err -> error $ "Error parsing file: " ++ show err
        Right exprs -> do
            currentEnv <- get
            (result, _) <- liftIO $ runEval (evalBlock exprs) currentEnv
            return result
eval (Literal v) = return v
eval (Var name) = do
    env <- get
    case Map.lookup name env of
        Just v -> return v
        Nothing -> error $ "Undefined variable: " ++ name
eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 + i2)
        _ -> error "Type mismatch in addition"
eval (Sub e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 - i2)
        _ -> error "Type mismatch in subtraction"
eval (Mul e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 * i2)
        _ -> error "Type mismatch in multiplication"
eval (Div e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (i1 `div` i2)
        _ -> error "Type mismatch in division"
eval (LessThan e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (if i1 < i2 then 1 else 0)
        _ -> error "Type mismatch in less-than comparison"
eval (GreaterThan e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (if i1 > i2 then 1 else 0)
        _ -> error "Type mismatch in greater-than comparison"
eval (Equals e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (if i1 == i2 then 1 else 0)
        (VString s1, VInt i2) -> case stringToInt s1 of
            Just i1 -> return $ VInt (if i1 == i2 then 1 else 0)
            Nothing -> error "Type mismatch: cannot compare string to integer"
        (VString s1, VString s2) -> return $ VInt (if s1 == s2 then 1 else 0)
        _ -> error "Type mismatch in equality comparison"
eval (Assign name expr) = do
    value <- eval expr
    modify $ Map.insert name value
    return value
eval (FuncCall name args) = do
    env <- get
    case Map.lookup name env of
        Just (VFunc params body) -> do
            argVals <- mapM eval args
            let localEnv = Map.fromList (zip params argVals)
            modify $ Map.union localEnv
            result <- evalBlock body
            modify $ Map.difference env
            return result
        _ -> error $ "Undefined function: " ++ name
eval (FuncDef name params body) = do
    modify $ Map.insert name (VFunc params body)
    return $ VFunc params body
eval (Write exprs) = do
    values <- mapM eval exprs
    let output = intercalate " " $ map showValue values
    liftIO $ putStrLn output
    return $ VString output
eval (Import moduleName) = do
    liftIO $ putStrLn $ "Importing module: " ++ moduleName
    return $ VString "Module imported"
eval (While condition body) = do
    condValue <- eval condition
    case condValue of
        VInt 0 -> return VVoid  
        _ -> do
            evalBlock body
            eval (While condition body)  
eval (For var startExpr endExpr body) = do
    startVal <- eval startExpr
    endVal <- eval endExpr
    case (startVal, endVal) of
        (VInt start, VInt end) -> do
            let loop i
                    | i >= end = return VVoid
                    | otherwise = do
                        modify $ Map.insert var (VInt i)
                        evalBlock body
                        loop (i + 1)
            loop start
        _ -> error "Type mismatch in for loop range"
eval (Wait expr) = do
    delay <- eval expr
    case delay of
        VInt ms -> do
            liftIO $ threadDelay (ms * 1000)
            return VVoid
        _ -> error "Type mismatch in wait"
eval (If condExpr ifBody elseBody) = do
    condValue <- eval condExpr
    case condValue of
        VInt 0 -> maybe (return VVoid) evalBlock elseBody 
        _ -> evalBlock ifBody  

eval (GetLine promptExpr) = do
    promptValue <- eval promptExpr
    case promptValue of
        VString prompt -> do
            liftIO $ putStr prompt
            liftIO $ hFlush stdout 
            input <- liftIO getLine
            return $ VString input
        _ -> error "Type mismatch: getLine expects a string prompt"

eval DeskList = do
    files <- liftIO $ listDirectory "."
    liftIO $ mapM_ putStrLn files
    return VVoid

eval (DeskDel fileExpr) = do
    fileName <- eval fileExpr
    case fileName of
        VString file -> do
            liftIO $ removeFile file
            return VVoid
        _ -> error "deskdel expects a file path as a string"

eval (DeskCd dirExpr) = do
    dirName <- eval dirExpr
    case dirName of
        VString dir -> do
            liftIO $ setCurrentDirectory dir
            return VVoid
        _ -> error "deskCd expects a directory path as a string"

eval (DeskRename oldNameExpr newNameExpr) = do
    oldName <- eval oldNameExpr
    newName <- eval newNameExpr
    case (oldName, newName) of
        (VString old, VString new) -> do
            liftIO $ renameFile old new
            return VVoid
        _ -> error "deskre expects two file paths as strings"

eval (DeskMake fileNameExpr) = do
    fileName <- eval fileNameExpr
    case fileName of
        VString file -> do
            liftIO $ writeFile file "" 
            return VVoid
        _ -> error "deskMake expects a file name as a string"

eval (SaveAs locationExpr) = do
    location <- eval locationExpr
    case location of
        VString loc -> do
            liftIO $ saveFileToLocation loc "Default content"
            return VVoid
        _ -> error "saveAs expects a file location as a string"

evalBlock :: [Expr] -> Eval Value
evalBlock = foldM (\_ expr -> eval expr) VVoid

showValue :: Value -> String
showValue (VInt i) = show i
showValue (VString s) = s
showValue VVoid = "void"

runEval :: Eval a -> Env -> IO (a, Env)
runEval m env = runStateT m env
