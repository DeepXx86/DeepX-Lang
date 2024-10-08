module AST where

data Expr
  = Literal Value
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | Equals Expr Expr
  | Assign String Expr
  | FuncCall String [Expr]
  | FuncDef String [String] [Expr]
  | Write [Expr]
  | Import String
  | While Expr [Expr]
  | For String Expr Expr [Expr]
  | Wait Expr
  | If Expr [Expr] (Maybe [Expr])
  | GetLine Expr
  | DeskList              
  | DeskDel Expr        
  | DeskCd Expr        
  | DeskRename Expr Expr 
  | DeskMake Expr 
  | SaveAs Expr 
  | BotDiscord [Expr]
  | Get String 
  | Depth FilePath
  deriving (Show)

data Value
  = VInt Int
  | VString String
  | VFunc [String] [Expr]
  | VVoid
  deriving (Show)

instance Eq Value where
  (VInt a) == (VInt b) = a == b
  (VString a) == (VString b) = a == b
  (VFunc params1 _) == (VFunc params2 _) = params1 == params2 
  _ == _ = False
