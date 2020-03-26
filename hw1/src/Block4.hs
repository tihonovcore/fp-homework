module Block4
  ( stringSum
  ) where

import Data.Char

-----------------------------
-- Task 1 -------------------
-----------------------------

stringSum :: String -> Maybe Int
stringSum s = fmap sum $ traverse (parseInt (Just 0)) (words s)
  where
    parseInt :: Maybe Int -> String -> Maybe Int
    parseInt Nothing _ = Nothing
    parseInt x [] = x
    parseInt (Just pref) (x : xs) | isDigit x = parseInt (Just $ pref * 10 + digitToInt x) xs
                                  | otherwise = Nothing

-----------------------------
-- Task 2 -------------------
-----------------------------

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap f (Leaf v    ) = Leaf (f v)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf

  (<*>) (Leaf f    ) args = fmap f args
  (<*>) (Branch l r) args = Branch (l <*> args) (r <*> args)

instance Foldable Tree where
  foldr f init (Leaf v) = f v init
  foldr f init (Branch l r) =
    let rightResult = (foldr f init r) in
      foldr f rightResult l

instance Traversable Tree where
  traverse f (Leaf v) = fmap Leaf (f v)
  traverse f (Branch l r) = (fmap Branch (traverse f l)) <*> (traverse f r)


-----------------------------
-- Task 3 -------------------
-----------------------------

data NonEmpty a = a :| [a]

joinNonEmpty :: [NonEmpty a] -> [a]
joinNonEmpty list = foldl f [] list
  where
    f :: [a] -> NonEmpty a -> [a]
    f resultList (x :| xs) = resultList ++ (x : xs)  

instance Functor NonEmpty where
  fmap f (first :| other) = (f first) :| (fmap f other)

instance Applicative NonEmpty where
  pure x = x :| []

  (<*>) (f :| fs) (x :| xs) = (f x) :| ((fmap f xs) ++ (fs <*> (x : xs)))

instance Foldable NonEmpty where
  foldr f init (first :| other) = f first (foldr f init other)

instance Traversable NonEmpty where
  traverse f (first :| other) =
    let first1 = fmap (:|) (f first) in --f ([b] -> NonEmpty b)
    let other1 = (traverse f other)  in --f [b]
      (<*>) first1 other1

instance Monad NonEmpty where
  return x = (x :| [])
  
  (>>=) (x :| xs) f = 
    case f x of 
      (y :| ys) -> (y :| (ys ++ (joinNonEmpty $ fmap f xs)))
