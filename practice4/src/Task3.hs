module Task3 where

data Expr a
  = Const a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)

data Statement a
  = Seq (Statement a) (Statement a)
  | EvalAndPrint (Expr a)
  | Log String

evalBinaryExpr :: (Num a) => Expr a -> Expr a -> (a -> a -> a) -> a
evalBinaryExpr l r op = (evalExpr l) `op` (evalExpr r)

evalExpr :: (Num a) => Expr a -> a
evalExpr (Const v) = v
evalExpr (Add l r) = evalBinaryExpr l r (+)
evalExpr (Sub l r) = evalBinaryExpr l r (-)
evalExpr (Mul l r) = evalBinaryExpr l r (*)

evalStatement :: (Show a, Num a) => Statement a -> String
evalStatement (Seq l r) = (evalStatement l) ++ (evalStatement r)
evalStatement (EvalAndPrint e) = show (evalExpr e)
evalStatement (Log s) = s
