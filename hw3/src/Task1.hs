{-# LANGUAGE BangPatterns #-}
module Task1 where

-- | Representation of two-dimensional point
data Point = Point
           { x :: !Int
           , y :: !Int
           }
  deriving (Show, Eq)

-- | Sums two points by coordinates
plus :: Point -> Point -> Point
plus a b = Point (x a + x b) (y a + y b)

-- | Subtract two points by coordinates
minus :: Point -> Point -> Point
minus a b = Point (x a - x b) (y a - y b)

-- | Evaluate distance between points
size :: Point -> Double
size p = sqrt . fromIntegral $ (x p ^ 2) + (y p ^ 2)

-- | Evaluate scalar product of two points
scalarProduct :: Point -> Point -> Int
scalarProduct a b = (x a * x b) + (y a * y b)

-- | Evaluate cross product of two points
crossProduct :: Point -> Point -> Int
crossProduct a b = (x a * y b) - (x b * y a)

-- | Evaluate perimeter of polygon without self-intersections strictly
perimeter :: [Point] -> Double
perimeter []           = 0
perimeter (first : list) = countPerimeter 0 first list
  where
    countPerimeter :: Double -> Point -> [Point] -> Double
    countPerimeter !accum prev (p : other) = countPerimeter (accum + size (minus prev p)) p other
    countPerimeter !accum prev []           = accum + size (minus first prev)

-- | Evaluate doubled area of polygon without self-intersections strictly
doubleArea :: [Point] -> Int
doubleArea []          = 0
doubleArea (first : list) = evalDoubleArea 0 first list
  where
    evalDoubleArea :: Int -> Point -> [Point] -> Int
    evalDoubleArea !accum prev (p : other) = evalDoubleArea (accum + trapezoidDoubleArea prev p) p other
    evalDoubleArea !accum prev []          = accum + trapezoidDoubleArea prev first

    trapezoidDoubleArea :: Point -> Point -> Int
    trapezoidDoubleArea p1 p2 = (y p1 + y p2) * (x p2 - x p1)

-- | Evaluate perimeter of polygon without self-intersections __not__ strictly
naivePerimeter :: [Point] -> Double
naivePerimeter []                         = 0
naivePerimeter (firstPoint : otherPoints) = evalNaivePerimeter firstPoint otherPoints
  where
    evalNaivePerimeter :: Point -> [Point] -> Double
    evalNaivePerimeter prev (p : other) = size (minus prev p) + evalNaivePerimeter p other
    evalNaivePerimeter prev []          = size (minus firstPoint prev)

-- | Evaluate doubled area of polygon without self-intersections __not__ strictly
naiveDoubleArea :: [Point] -> Int
naiveDoubleArea []          = 0
naiveDoubleArea (first : list) = evalNaiveDoubleArea first list
  where
    evalNaiveDoubleArea :: Point -> [Point] -> Int
    evalNaiveDoubleArea prev (p : other) = trapezoidDoubleArea prev p + evalNaiveDoubleArea p other
    evalNaiveDoubleArea prev []          = trapezoidDoubleArea prev first

    trapezoidDoubleArea :: Point -> Point -> Int
    trapezoidDoubleArea p1 p2 = (y p1 + y p2) * (x p2 - x p1)
