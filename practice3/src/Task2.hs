module Task2 where

-- #####################################
-- Task 6 ##############################

data Point3D a = Point3D a a a
  deriving Show

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

instance Applicative Point3D where
  pure x = Point3D x x x
  (<*>) (Point3D fx fy fz) (Point3D x y z) = Point3D (fx x) (fy y) (fz z)

-- #####################################
-- Task 7 ##############################

data Tree a
  = Leaf (Maybe a)
  | Branch (Tree a) (Maybe a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf value) = Leaf (fmap f value)
  fmap f (Branch left value right) = Branch (fmap f left) (fmap f value) (fmap f right)

--instance Applicative Tree where
--  pure value = Leaf value
--  (<*>) (Leaf f) (Leaf oldValue) = Leaf (f oldValue)
--  (<*>) (Leaf f) (Branch left oldValue right) = Branch ((Leaf f) <*> left) (f oldValue) ((Leaf f) <*> right)

-- #####################################
-- Task 8 ##############################

--(<$) :: Functor f => a -> f b -> f a
--(<$)

--(*>) :: Applicative f => f a -> f b -> f b
--(*>)

-- #####################################
-- Task 9 ##############################

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

