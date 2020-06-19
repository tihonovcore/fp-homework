module ComonadUtils where

import Control.Comonad (Comonad(..))

data ListZipper a = LZ [a] a [a]
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (l : ll) c r) = LZ ll l (c : r)
listLeft (LZ []       _ _) = error "no left elems"

listRight :: ListZipper a -> ListZipper a
listRight (LZ l c (r : rr)) = LZ (c : l) r rr
listRight (LZ _ _ []      ) = error "no right elems"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite new (LZ l _ r) = LZ l new r

gridShow :: Show a => Grid a -> Grid String
gridShow = fmap show

listShowN :: Int -> ListZipper String -> String
listShowN n (LZ l c r) = concat (reverse (take n l) ++ [c] ++ take n r)

gridShowN :: Int -> Grid String -> String
gridShowN n (Grid g) = listShowN n $ fmap ((++) "\n" . listShowN n) g

instance Functor ListZipper where
  fmap f (LZ l c r) = LZ (fmap f l) (f c) (fmap f r)

instance Comonad ListZipper where
  extract (LZ _ c _) = c
  duplicate = genericMove listLeft listRight

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove f g s = LZ (iterateTail f s) s (iterateTail g s)

up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

left :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite a (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite a oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical   = genericMove up   down

instance Functor Grid where
  fmap f (Grid g) = Grid $ fmap f <$> g

instance Comonad Grid where
  extract = gridRead
  duplicate = Grid . fmap horizontal . vertical
