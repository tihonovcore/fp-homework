module Block1
  ( BinaryTree (..)
  , Day (..)
  , Nat (..)
  , Values (..)
  , afterDays
  , daysToParty
  , find
  , fromIntToNat
  , fromList
  , isEmpty
  , isEven
  , isWeekend
  , mult
  , natDiv
  , natMod
  , nextDay
  , plus
  , put
  , remove
  , size
  , subs
  ) where

-----------------------------
-- Task 1 -------------------
-----------------------------

-- |Interpret days of week
data Day = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Eq Day where
  (==) Monday Monday = True
  (==) Tuesday Tuesday = True
  (==) Wednesday Wednesday = True
  (==) Thursday Thursday = True
  (==) Friday Friday = True
  (==) Saturday Saturday = True
  (==) Sunday Sunday = True
  (==) _ _ = False

-- |Find name of next day in a week, or first of week
-- day in case of last day of week
nextDay :: Day -> Day
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

-- |Find day which come after n days
afterDays :: Day -> Int -> Day
afterDays day n = nextNDay day (mod ((mod n weekLen) + weekLen) weekLen)
  where
    weekLen :: Int
    weekLen = 7

    nextNDay :: Day -> Int -> Day
    nextNDay day 0 = day
    nextNDay day n = nextDay $ nextNDay day (n - 1)

-- |Check that day is weekend
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- |Counts days to Friday
daysToParty :: Day -> Int
daysToParty = findDaysToParty 0
  where
    findDaysToParty :: Int -> Day -> Int
    findDaysToParty n Friday = n
    findDaysToParty n day    = findDaysToParty (n + 1) $ nextDay day


-----------------------------
-- Task 2 -------------------
-----------------------------

-- |Interprets natural numbers
data Nat = Z | S Nat
  deriving (Show)

instance Eq Nat where
  (==) Z     Z     = True
  (==) (S x) (S y) = x == y
  (==) _     _     = False

instance Ord Nat where
  compare Z Z = EQ
  compare Z _ = LT
  compare _ Z = GT
  compare (S x) (S y) = compare x y

-- |Find amount of two Nat
plus :: Nat -> Nat -> Nat
plus Z        right = right
plus (S left) right = S (plus left right)

-- |Find composition of two Nat
mult :: Nat -> Nat -> Nat
mult Z _ = Z
mult (S left) right = plus right $ mult left right

-- |Nat subtract operation
subs :: Nat -> Nat -> Nat
subs Z    _ = Z
subs left Z = left
subs (S left) (S right) = subs left right

-- |Convert Int to Nat
fromIntToNat :: Int -> Nat
fromIntToNat x | x > 0     = S (fromIntToNat $ x - 1)
               | otherwise = Z

-- |Check that number n is even, i.e. exists k: k * 2 = n
isEven :: Nat -> Bool
isEven Z = True
isEven (S x) = not $ isEven x

-- |Nat division operation
natDiv :: Nat -> Nat -> Nat
natDiv x y = findResult x Z
  where
    findResult :: Nat -> Nat -> Nat
    findResult x k =
      case compare x y of
        EQ -> plus k (S Z)
        LT -> k
        GT -> findResult (subs x y) (S k)

-- |Find k what exists t: y * t + k = x
natMod :: Nat -> Nat -> Nat
natMod x y = subs x (mult (natDiv x y) y)


-----------------------------
-- Task 3 -------------------
-----------------------------

-- |Non empty list
data Values a = Values a [a]
  deriving (Show)

instance Eq a => Eq (Values a) where
  (Values x xs) == (Values y ys) = (x == y) && (xs == ys)

-- |Implementation of Binary Tree Search
data BinaryTree a = Leaf | Node (BinaryTree a) (BinaryTree a) (Values a)
  deriving (Show)

instance Eq a => Eq (BinaryTree a) where
  Leaf == Leaf = True
  Leaf == _    = False
  _    == Leaf = False
  (Node l1 r1 v1) == (Node l2 r2 v2) = (l1 == l2) && (r1 == r2) && (v1 == v2) 

-- |Check that tree has at least one value
isEmpty :: BinaryTree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- |Count values in tree
size :: BinaryTree a -> Int
size Leaf = 0
size (Node l r (Values _ v)) = (size l) + (size r) + (1 + length v)

-- |Returns True if tree has required element
find :: (Ord a) => a -> BinaryTree a -> Bool
find _ Leaf = False
find x (Node l r (Values value _)) =
  case compare x value of
    EQ -> True
    LT -> find x l
    GT -> find x r

-- |Add element x to tree
put :: (Ord a) => a -> BinaryTree a -> BinaryTree a
put x Leaf = Node Leaf Leaf (Values x [])
put x (Node l r values@(Values value other)) =
  case compare x value of
    EQ -> (Node l r (Values value (x : other)))
    LT -> (Node (put x l) r values)
    GT -> (Node l (put x r) values)

-- |Build tree by list
fromList :: (Ord a) => [a] -> BinaryTree a
fromList = fromListWithAccum Leaf
  where
    fromListWithAccum :: (Ord a) => BinaryTree a -> [a] -> BinaryTree a
    fromListWithAccum accum []       = accum
    fromListWithAccum accum (x : xs) = fromListWithAccum (put x accum) xs

-- |Remove element if it exists
remove :: (Ord a) => a -> BinaryTree a -> BinaryTree a
remove _ Leaf = Leaf
remove e (Node l r values@(Values x xs)) =
  case compare e x of
    LT -> Node (remove e l) r values
    GT -> Node l (remove e r) values 
    EQ -> case xs of
            (y : ys) -> Node l r (Values x ys)
            []       -> up l r
              where 
                up :: BinaryTree a -> BinaryTree a -> BinaryTree a
                up Leaf Leaf  = Leaf
                up Leaf right = right
                up left Leaf  = left
                up (Node c1 c2 v) right = Node (up c1 c2) right v
