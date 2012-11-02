module Ast where

data MLOp = Add | Sub | Mul | Div | Fst | Snd | OpIf | OpFix
          deriving (Show)

nameOp Add = "+"
nameOp Sub = "-"
nameOp Mul = "*"
nameOp Div = "/"
nameOp Fst = "fst"
nameOp Snd = "snd"
nameOp OpIf = "opif"
nameOp OpFix = "opfix"

newtype MLIdent = MLIdent String
                deriving (Show, Eq, Ord)

data MLExpr = MLInt Integer
            | MLBool Bool
            | MLPair MLExpr MLExpr
            | MLVar MLIdent
            | MLPrim MLOp
            | MLLet { letVar :: MLIdent, letValue :: MLExpr, letBody :: MLExpr }
            | MLFun { funArg :: MLIdent, funBody :: MLExpr }
            | MLApp MLExpr MLExpr
            deriving (Show)