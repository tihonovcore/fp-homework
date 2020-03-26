module Block5
  ( ArithmeticError (..)
  , Expr (..)
  , eval
  ) where

-----------------------------
-- Task 1 -------------------
-----------------------------

-- |Implementation of arithmetical expressions
data Expr = Const Int
          | Plus     Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide   Expr Expr
          | Pow      Expr Expr

-- |Evaluation errors for `Expr`
data ArithmeticError
  = DivisionByZero
  | NegativePowArgument
  deriving (Show)

instance Eq ArithmeticError where
  DivisionByZero      == DivisionByZero      = True
  NegativePowArgument == NegativePowArgument = True
  _                   == _                   = False

--TODO попробоваться через монадки
-- |Evaluate expression. Result is `Right x` or
-- `Left error` if error exists
eval :: Expr -> Either ArithmeticError Int
eval (Const    v  ) = Right v
--eval (Plus     l r) = eval l >>= \lval -> eval r >>= \rval -> Right $ lval + rval     <=== Monad
eval (Plus     l r) = (fmap (+)   (eval l)) <*> (eval r)
eval (Subtract l r) = (fmap (-)   (eval l)) <*> (eval r)
eval (Multiply l r) = (fmap (*)   (eval l)) <*> (eval r)
eval (Divide   l r) = (fmap (div) (eval l)) <*> (check (\t -> t /= 0) (eval r) DivisionByZero     )
eval (Pow      l r) = (fmap (^)   (eval l)) <*> (check (\t -> t >= 0) (eval r) NegativePowArgument)

check :: (a -> Bool) -> Either ArithmeticError a -> ArithmeticError -> Either ArithmeticError a
check f l@(Left _) _ = l
check f r@(Right x) e =
  case f x of
    True  -> r
    False -> Left e

-----------------------------
-- Task 2 -------------------
-----------------------------

