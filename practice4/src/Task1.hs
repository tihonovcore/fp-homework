module Task1 where

-- #####################################
-- ### Task 1 ##########################
-- #####################################

import Data.Map (Map, (!), empty, insert)

data Expr a
  = Const a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)

data ErrMsg
  = DivisionByZero

evalBinaryExpr :: (Integral a) => Expr a -> Expr a -> (a -> a -> a) -> Either ErrMsg a
evalBinaryExpr l r op = do
  lvalue <- evalExpr l
  rvalue <- evalExpr r
  return $ op lvalue rvalue

evalExpr :: (Integral a) => Expr a -> Either ErrMsg a
evalExpr (Const v) = Right v
evalExpr (Add l r) = evalBinaryExpr l r (+)
evalExpr (Sub l r) = evalBinaryExpr l r (-)
evalExpr (Mul l r) = evalBinaryExpr l r (*)
evalExpr (Div l r) = evalBinaryExpr l r (div)
evalExpr (Pow l r) = evalBinaryExpr l r (^)


-- #####################################
-- ### Task 2 ##########################
-- #####################################

data Expr2 a
  = Const2 a
  | Var2 String
  | Add2 (Expr2 a) (Expr2 a)
  | Sub2 (Expr2 a) (Expr2 a)
  | Mul2 (Expr2 a) (Expr2 a)

type VarMap a = Map String a

evalBinaryExpr2 :: (Num a) => Expr2 a -> Expr2 a -> VarMap a -> (a -> a -> a) -> a
evalBinaryExpr2 l r m op = evalExpr2 l m `op` evalExpr2 r m 

evalExpr2 :: (Num a) => Expr2 a -> VarMap a -> a
evalExpr2 (Const2 v) _ = v
evalExpr2 (Var2 var) m = m ! var 
evalExpr2 (Add2 l r) m = evalBinaryExpr2 l r m (+) 
evalExpr2 (Sub2 l r) m = evalBinaryExpr2 l r m (-) 
evalExpr2 (Mul2 l r) m = evalBinaryExpr2 l r m (*)
