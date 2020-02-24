module Task3
  ( composition
  , contraction
  , identity
  , permutation
  , s
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- B-combinator
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- s k k x = (k x) (k x) = x
identity :: a -> a
identity = s const const

-- s s false f x = s (s f) (false f) x = s f id x = f x (id x) = f x x
contraction :: (a -> a -> b) -> a -> b
contraction = s s false
  where
    -- k id = \x.(\y.y)
    false :: a -> b -> b
    false = const identity

-- C-combinator
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)
