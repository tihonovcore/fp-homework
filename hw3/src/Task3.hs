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
  Int32   :: Int -> Expression Int
  Boolean :: Bool -> Expression Bool
  Str     :: String -> Expression String
  Dbl     :: Double -> Expression Double

  Fun     :: Typeable t
          => (Expression t -> Expression k)
          -> ((Expression t -> Expression k) -> Expression ()) 
          -> Expression ()
  Fun2    :: (Typeable t1, Typeable t2)
          => (Expression t1 -> Expression t2 -> Expression k) 
          -> ((Expression t1 -> Expression t2 -> Expression k) -> Expression ()) 
          -> Expression ()
  
  Plus    :: Expression Int    -> Expression Int    -> Expression Int
  Subs    :: Expression Int    -> Expression Int    -> Expression Int
  Mult    :: Expression Int    -> Expression Int    -> Expression Int
  Mod     :: Expression Int    -> Expression Int    -> Expression Int
  Gt      :: Ord t => Expression t    -> Expression t    -> Expression Bool
  And     :: Expression Bool   -> Expression Bool   -> Expression Bool
  Conc    :: Expression String -> Expression String -> Expression String
  
  If      :: Expression Bool -> Then t -> Else t -> Expression t
  While   :: Expression Bool -> Expression () -> Expression ()
  
  Variable :: Typeable t => String -> IORef (Expression t) -> Expression t
  Var        :: Typeable t => (Expression t -> Expression ()) -> Expression ()
  
  Apply  :: Expression t -> Expression t -> Expression () 
  Print  :: Show t => Expression t -> Expression ()
  
  Seq :: Expression a -> Expression b -> Expression b
  End :: Expression ()

interpret :: Expression a -> IO a
interpret (Int32   v) = return v
interpret (Boolean v) = return v
interpret (Dbl     v) = return v
interpret (Str     v) = return v

interpret (Fun  f scope) = interpret $ scope f
interpret (Fun2 f scope) = interpret $ scope f

interpret (Plus l r) = interpretBinOp l r (Prelude.+)
interpret (Subs l r) = interpretBinOp l r (Prelude.-)
interpret (Mult l r) = interpretBinOp l r (Prelude.*)
interpret (Mod  l r) = interpretBinOp l r Prelude.mod
interpret (Gt   l r) = interpretBinOp l r (Prelude.>)
interpret (And  l r) = interpretBinOp l r (Prelude.&&)
interpret (Conc l r) = interpretBinOp l r (Prelude.++)

interpret (If cond (Then t) (Else e)) = do
  c <- interpret cond
  if c then interpret t else interpret e
interpret (While cond action) = do
  c <- interpret cond
  when c $ interpret action >> interpret (While cond action)

interpret (Variable _ e) = do
  value <- readIORef e
  interpret value
interpret (Var    f) = do
  fake <- newIORef 0
  dv   <- defaultValue fake f
  interpret $ f dv

interpret (Apply (Variable _ value) boxedNewValue) = do
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
unbox :: Expression a -> IO (Expression a)
unbox x@(Int32  _) = return x
unbox x@(Boolean _) = return x
unbox x@(Str     _) = return x
unbox x@(Dbl     _) = return x
unbox (Plus l r) = unboxBinOp l r Plus
unbox (Subs l r) = unboxBinOp l r Subs
unbox (Mult l r) = unboxBinOp l r Mult
unbox (Mod  l r) = unboxBinOp l r Mod
unbox (Gt   l r) = unboxBinOp l r Gt
unbox (And  l r) = unboxBinOp l r And
unbox (Conc l r) = unboxBinOp l r Conc
unbox (Variable _ io) = readIORef io
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

-- TODO: override + % etc

-- | Default values for primitive types
defaultValue :: Typeable t => IORef Int -> (Expression t -> Expression k) -> IO (Expression t)
defaultValue index (_ :: Expression t -> Expression k) = do
  res <- newIORef $ fromMaybe undefined $ defInt <|> defBool <|> defStr <|> defDbl
  currIndex <- readIORef index
  writeIORef index (currIndex + 1)
  return $ Variable ("var" <> show currIndex) res
  where
    defInt :: Maybe (Expression t)
    defInt = do
      Refl <- eqT @t @Int
      pure $ Int32 0

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
