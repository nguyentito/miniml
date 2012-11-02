module Ast where

data MLOp = Add | Sub | Mul | Div | Fst | Snd | OpIf | OpFix

nameOp Add = "+"
nameOp Sub = "-"
nameOp Mul = "*"
nameOp Div = "/"
nameOp Fst = "fst"
nameOp Snd = "snd"
nameOp OpIf = "opif"
nameOp OpFix = "opfix"

data MLExpr = MLInt Integer
            | MLBool Bool
            | MLPair MLExpr MLExpr
            | MLVar String
            | MLPrim MLOp
            | MLLet { letVar :: MLVar, letValue :: MLExpr, letBody :: MLExpr }
            | MLFun { funArg :: MLVar, funBody :: MLExpr }
            | MLApp MLExpr MLExpr
