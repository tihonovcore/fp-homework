module Task1 where

-- #####################################
-- Task 1 ##############################

newtype Last a = Last (Maybe a)

instance Semigroup (Last a) where
  (<>) (Last l) (Last r) =
    case r of
      Just _  -> (Last r)
      Nothing -> (Last l)

instance Monoid (Last a) where
  mempty = Last Nothing

-- #####################################
-- Task 2 ##############################

mconcat :: Monoid m => [m] -> m
mconcat []       = mempty
mconcat (x : xs) = x <> (Task1.mconcat xs)

-- #####################################
-- Task 3 ##############################

foldMap :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap f = foldr (\a m -> (<>) (f a) m) mempty
-- foldr :: (a -> b -> b) -> b -> t a -> b
-- foldr :: (a -> m -> m) -> m -> t a -> m

-- #####################################
-- Task 4 ##############################

--instance Foldable [] where
--  foldr f accum []       = accum
--  foldr f accum (x : xs) = f x (fold accum xs)

-- #####################################
-- Task 5 ##############################

--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--foldr f accum obj = foldMap
--foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m