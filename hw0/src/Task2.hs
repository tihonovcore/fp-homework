module Task2
  ( axiom9
  , contraposition
  , doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- We know that `flip id :: b -> (b -> c) -> c`.
-- If change `c` to `Void`, we get `flip id :: b -> (b -> Void) -> Void`.
-- It's equals to `b -> Neg (Neg b)`
doubleNeg :: a -> Neg (Neg a)
doubleNeg = flip (\x -> x)

-- contraposition axiom6 = !(a or !a) -> !a
-- contraposition axiom7 = !(a or !a) -> !!a
-- => !!(a or !a)
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = axiom9 (contraposition axiom6) (contraposition axiom7)
  where
    axiom6 :: a -> Either a b
    axiom6 a = Left a

    axiom7 :: b -> Either a b
    axiom7 b = Right b

-- Let [a] = (1; 2) and [b] = (3, 4)
-- 1) [a -> b] = (-inf; 1) or (2; +inf)
-- 2) [(1) -> a] = max { x | x and [(-inf; 1) or (2; +inf)] <= (1; 2)} = R
-- 3) [(2) -> a] = max { x | x and R <= (1; 2)} = R \ (1; 2) != R
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- Let [a] = R \ {0}
-- 1) [Neg a] = empty
-- 2) [Neg (Neg a)] = R
-- 3) [(2) -> a] = max { x | x and R <= R \ {0} } = R \ {0} != R
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- We can use synonym to `Neg a`: (a -> b) -> (a -> (b -> Void)) -> a -> Void
-- Let f = (a -> b) $ a :: b
-- Let g = (a -> (b -> Void)) $ a :: b -> Void
-- With MP of g and f is Void
axiom9 :: (a -> b) -> (a -> Neg b) -> Neg a
axiom9 f g a = (g a) (f a)

contraposition :: (a -> b) -> (Neg b -> Neg a)
contraposition f notB a = notB (f a)

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = contraposition doubleNeg
