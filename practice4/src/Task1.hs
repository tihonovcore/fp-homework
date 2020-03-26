module Task1 where

-- #####################################
-- Task 1 ##############################

data Expr a
  = Const a
  | MinusInfinity
  | PlusInfinity
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)

data ErrMsg
  = DivisionByZero

instance Monad Expr where
  return a = Const a
  (>>=)

evalExpr :: (Num a) => Expr a -> Either ErrMsg a
evalExpr = undefined