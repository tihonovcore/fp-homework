{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Task3 where

import Data.Typeable ((:~:) (..), Typeable, eqT)
--import Prelude hiding ((+), (>), (&&))
--import qualified Prelude
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Control.Monad.State (State, state)
import GHC.TypeLits (Symbol)
import Data.IORef (newIORef, IORef, readIORef, writeIORef)

data SomeE where
  SomeE :: Expression t -> SomeE

data Expression a where
  Number :: Int -> Expression Int
  Boolean :: Bool -> Expression Bool
  Plus   :: Expression Int  -> Expression Int  -> Expression Int
  Gt     :: Expression Int  -> Expression Int  -> Expression Bool
  And    :: Expression Bool -> Expression Bool -> Expression Bool
  While  :: Expression Bool -> Expression t -> Expression t
--  Var' :: Typeable t => State (Expression t) (Expression t) -> Expression t
  IOVariable :: Typeable t => String -> IORef (Expression t) -> Expression t
  RRVariable :: Typeable t => String -> Expression t -> Expression t
  Var    :: Typeable t => (Expression t -> Expression ()) -> Expression () --TODO
  Apply  :: Expression t -> Expression t -> Expression () --TODO
  Print  :: Show t => Expression t -> Expression ()
  Seq :: Expression a -> Expression b -> Expression b --TODO
  End :: Expression ()

interpret :: Expression a -> IO a
interpret (Number n) = return n
interpret (Boolean   n) = return n
interpret (Plus l r) = do
  ll <- interpret l
  rr <- interpret r
  return $ ll Prelude.+ rr
interpret (Gt   l r) = do
  ll <- interpret l
  rr <- interpret r
  return $ ll Prelude.> rr
interpret (And  l r) = do
  ll <- interpret l
  rr <- interpret r
  return $ ll Prelude.&& rr
interpret (IOVariable _ e) = do
  value <- readIORef e
  interpret value
interpret (Var    f) = do
  dv <- defValue f
  interpret $ f dv
interpret (Print  e) = do
  ee <- interpret e
  print ee
interpret (Apply (IOVariable _ value) newValue) = writeIORef value newValue
interpret (Seq  l r) = interpret l >> interpret r
interpret End = return ()

---- TODO: 
--interpretVar :: Expression t -> IO ()
--interpretVar (Var f) = do
--  let v = defValue f
--  ref <- newIORef v
--  let res = fmap f ref
--  res

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
--      Var (\y ->
--        y @= Boolean True #
--        y @= y `And` Boolean True #
--        End
--      ) #
--      While (Boolean True) (Number 2 `Plus` Number 4) #
      End

-- | Default values for primitive types
defValue :: Typeable t => (Expression t -> Expression k) -> IO (Expression t)
defValue (_ :: Expression t -> Expression k) = do
  let res = fromMaybe undefined $ defInt <|> defBool
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
