{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Task3 where

import Data.Typeable ((:~:) (..), Typeable, eqT)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.IORef (newIORef, IORef, readIORef, writeIORef)
import Control.Monad (when)

data SomeE where
  SomeE :: Expression t -> SomeE

newtype Then t = Then (Expression t)
newtype Else t = Else (Expression t)

data Expression a where
  Number  :: Int -> Expression Int
  Boolean :: Bool -> Expression Bool
  Str     :: String -> Expression String
  Dbl     :: Double -> Expression Double

  --TODO: functions
  
  Plus    :: Expression Int  -> Expression Int  -> Expression Int
  Gt      :: Expression Int  -> Expression Int  -> Expression Bool
  And     :: Expression Bool -> Expression Bool -> Expression Bool
  
  If      :: Expression Bool -> Then t -> Else t -> Expression t
  While   :: Expression Bool -> Expression () -> Expression ()
  
  IOVariable :: Typeable t => String -> IORef (Expression t) -> Expression t
  RRVariable :: Typeable t => String -> Expression t -> Expression t
  Var        :: Typeable t => (Expression t -> Expression ()) -> Expression ()
  
  Apply  :: Expression t -> Expression t -> Expression () 
  Print  :: Show t => Expression t -> Expression ()
  
  Seq :: Expression a -> Expression b -> Expression b
  End :: Expression ()

interpret :: Expression a -> IO a
interpret (Number  v) = return v
interpret (Boolean v) = return v
interpret (Dbl     v) = return v
interpret (Str     v) = return v

interpret (Plus l r) = interpretBinOp l r (Prelude.+)
interpret (Gt   l r) = interpretBinOp l r (Prelude.>)
interpret (And  l r) = interpretBinOp l r (Prelude.&&)

interpret (If cond (Then t) (Else e)) = do
  c <- interpret cond
  if c then interpret t else interpret e
interpret (While cond action) = do
  c <- interpret cond
  when c $ interpret action >> interpret (While cond action)

interpret (IOVariable _ e) = do
  value <- readIORef e
  interpret value
interpret (RRVariable _ _) = error "impossible"
interpret (Var    f) = do
  dv <- defValue f
  interpret $ f dv

interpret (Apply (IOVariable _ value) boxedNewValue) = do
  newValue <- unbox boxedNewValue
  writeIORef value newValue
interpret (Apply _ _) = error "impossible"
interpret (Print  e) = do
  ee <- interpret e
  print ee

interpret (Seq  l r) = interpret l >> interpret r
interpret End = return ()

interpretBinOp :: Expression t -> Expression t -> (t -> t -> r) -> IO r
interpretBinOp l r op = do
  ll <- interpret l
  rr <- interpret r
  return $ ll `op` rr

-- | Transform expression with variables to expression
-- with their values
-- 
-- Example:
-- x = 100
-- y = 333
-- 2 * x + y --> 2 * 100 + 333
unbox :: Expression a -> IO (Expression a) -- TODO unbox остального
unbox x@(Number  _) = return x
unbox x@(Boolean _) = return x
unbox x@(Str     _) = return x
unbox x@(Dbl     _) = return x
unbox (Plus l r) = unboxBinOp l r Plus
unbox (Gt   l r) = unboxBinOp l r Gt
unbox (And  l r) = unboxBinOp l r And
unbox (IOVariable _ io) = readIORef io
unbox _ = error "internal error"

unboxBinOp :: Expression t -> Expression t -> (Expression t -> Expression t -> Expression r) -> IO (Expression r)
unboxBinOp l r op = do
  ll <- unbox l
  rr <- unbox r
  return $ ll `op` rr

infixr 7 #
(#) :: Expression a -> Expression b -> Expression b
(#) = Seq

infix 8 @=
(@=) :: Expression t -> Expression t -> Expression ()
(@=) = Apply

bar :: Expression ()
bar =
      Var (\a -> 
        a @= Number 2018 #
        Print (a `Plus` Number 10)
      ) #
      Var (\x ->
        x @= Number (-10) #
--        While (x `Gt` (x `Plus` x)) ( x @= x `Plus` Number 1 ) #
        Var (\y ->
          y @= x #
          Print y
        ) #
        End
      ) #
      Var (\y ->
        y @= Number 0 #
        While (Number 5 `Gt` y) (
          Var (\x ->
            (If   $ x `Gt` y)
            (Then $ Print  y)
            (Else $ Print  x) #
            x @= y `Plus` Number 1 #
            y @= x
--            Print y
          )
        ) #
--        y @= y `And` Boolean True #
        End
      ) #
--      While (Boolean True) (Number 2 `Plus` Number 4) #
      End

-- | Default values for primitive types
defValue :: Typeable t => (Expression t -> Expression k) -> IO (Expression t)
defValue (_ :: Expression t -> Expression k) = do
  let res = fromMaybe undefined $ defInt <|> defBool <|> defStr <|> defDbl
  ref <- newIORef res
  return $ IOVariable "v0" ref
  where
    defInt :: Maybe (Expression t)
    defInt = do
      Refl <- eqT @t @Int
      pure $ Number 0

    defBool :: Maybe (Expression t)
    defBool = do
      Refl <- eqT @t @Bool
      pure $ Boolean False
    
    defStr :: Maybe (Expression t)
    defStr = do
      Refl <- eqT @t @String
      pure $ Str ""
    
    defDbl :: Maybe (Expression t)
    defDbl = do
      Refl <- eqT @t @Double
      pure $ Dbl 0
