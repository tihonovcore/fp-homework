module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function

iterateElement :: a -> [a]
iterateElement = fix (\f a -> a : f a)

fibonacci :: Integer -> Integer
fibonacci = fix (\f n -> if n <= 2 then 1 else (f (n - 1)) + (f (n - 2)))

factorial :: Integer -> Integer
factorial = fix (\f n -> if n == 0 then 1 else n * f (n - 1))

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\f m a -> case a of []   -> []
                                  x:xs -> m x : f m xs)
