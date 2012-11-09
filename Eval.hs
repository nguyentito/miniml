module Eval (MLValue, mlEval) where

import Control.Monad.Either
import qualified Data.Map as Map

import Ast

--- A data type for values
--- i.e. normal forms considered the result of a succesful computation ---

data MLValue = VInt Integer | VBool Bool
             | VPair MLValue MLValue
             | VPrim MLOp
             | VFun Environment MLIdent MLExpr -- it's a closure !

type Environment = Map.Map MLIdent MLValue

eval :: Environment MLExpr -> Either String MLValue

eval env (MLInt i)    = pure (VInt i)
eval env (MLBool b)   = pure (VBool i)
eval env (MLPair x y) = VPair <*> (eval env x) <*> (eval env y)
eval env (MLVar id)   = case Map.lookup id env of
                           Nothing -> Left "undefined variable"
                           Just v -> Right v
eval env (MLPrim op)  = pure (VPrim op)
eval env e@(MLLet _)  = eval (Map.insert (letVar e) (letValue e) env) (letBody e)
eval env e@(MLFun _)  = VFun env (funArg e) (funBody e)

eval env (MLApp funExpr argExpr) = case eval env funExpr of
  VFun closure var body -> eval (Map.insert var (eval argExpr) closure) body
  VPrim Add -> undefined

mlEval :: MLExpr -> MLValue
mlEval = eval (Map.Map.empty) 