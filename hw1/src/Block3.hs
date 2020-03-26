module Block3
  ( maybeConcat
  , Name (..)
  , NonEmpty (..)
  , ThisOrThat (..)
  ) where

-----------------------------
-- Task 1 -------------------
-----------------------------

-- |Concatenate elements except Nothing
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = concat_ list
  where
    concat_ :: [Maybe [a]] -> [a]
    concat_ = foldr f []
      where
        f :: Maybe [a] -> [a] -> [a]
        f Nothing r = r
        f (Just l) r = l ++ r


-----------------------------
-- Task 2 -------------------
-----------------------------

-- |Implementation of non empty lists
data NonEmpty a = a :| [a]
  deriving (Show)

instance Eq a => Eq (NonEmpty a) where
  (x :| xs) == (y :| ys) = (x == y) && (xs == ys)

instance Semigroup (NonEmpty a) where
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ [y] ++ ys)

data ThisOrThat a b = This a | That b | Both a b
  deriving (Show)

instance Semigroup (ThisOrThat a b) where
  (<>) (Both a b) _ = Both a b
  (<>) (This a) (This _) = This a
  (<>) (This a) (That b) = Both a b
  (<>) (That a) (That _) = That a
  (<>) (That a) (This b) = Both b a
  (<>) (That a) (Both b _) = Both b a
  (<>) (This a) (Both _ b) = Both a b

data Name = Empty | Name String
  deriving (Show)

instance Eq Name where
  Empty  == Empty  = True
  Name a == Name b = (a == b)
  _      == _      = False

-- |Concatenate Name's with '.'
instance Semigroup Name where
  (<>) Empty Empty = Empty
  (<>) Empty name = name
  (<>) name Empty = name
  (<>) (Name left) (Name right) = Name (left ++ "." ++ right)

instance Monoid Name where
  mempty = Empty
