module Evaluator where

import Control.Monad.State
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map
import Data.List (intercalate)

import AST

type Env = Map.Map String Value
type Eval a = StateT Env IO a

eval :: Expr -> Eval Value
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
        VInt 0 -> return VVoid  -- Stop the loop if the condition is 0 (false)
        _ -> do
            evalBlock body
            eval (While condition body)  -- Recurse until the condition is false
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
            liftIO $ threadDelay (ms * 1000)  -- Convert milliseconds to microseconds
            return VVoid
        _ -> error "Type mismatch in wait"
eval (If condExpr ifBody elseBody) = do
    condValue <- eval condExpr
    case condValue of
        VInt 0 -> maybe (return VVoid) evalBlock elseBody  -- If false, execute else body
        _ -> evalBlock ifBody  -- If true, execute if body


evalBlock :: [Expr] -> Eval Value
evalBlock = foldM (\_ expr -> eval expr) VVoid

showValue :: Value -> String
showValue (VInt i) = show i
showValue (VString s) = s
showValue (VFunc _ _) = "<function>"
showValue VVoid = ""

runEval :: Eval a -> Env -> IO (a, Env)
runEval ev env = runStateT ev env
