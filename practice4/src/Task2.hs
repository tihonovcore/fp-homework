module Task2 where

-- #####################################
-- Task 6 ##############################

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

half :: Int -> Writer String Int
half x = do
  tell $ "Halved " ++ (show x)
  return (x `div` 2)

x = runWriter

greeter :: Reader String String
greeter = do
  name <- ask
  return ("Hello, " ++ name)

y = runReader

foo :: State String String
foo = do
  name <- get
  put "tintin"
  return ("hello, " ++ name ++ "!")

z = runState