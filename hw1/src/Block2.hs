{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE InstanceSigs        #-}

module Block2
  ( FTree (..)
  , splitOn
  ) where

-----------------------------
-- Task 1 -------------------
-----------------------------

data FTree a = FLeaf a | FNode (FTree a) (FTree a) a

instance Foldable FTree where
  foldMap f (FLeaf v) = f v
  foldMap f (FNode l r v) = (foldMap f l) <> (f v) <> (foldMap f r)

  foldr f init (FLeaf v) = f v init
  foldr f init (FNode l r v) =
    let rightValue = foldr f init r in
    let midValue  = f v rightValue in
      foldr f midValue l


-----------------------------
-- Task 2 -------------------
-----------------------------

splitOn :: forall a. Eq a => a -> [a] -> [[a]]
splitOn ch list = reverse $ fmap reverse $ foldl f [[]] list
  where
    f :: Eq a => [[a]] -> a -> [[a]]
    f [] _ = []
    f (x : xs) elem | elem == ch = ([] : x : xs)
                    | elem /= ch = (elem : x) : xs
