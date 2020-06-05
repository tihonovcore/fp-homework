{-# LANGUAGE BangPatterns #-}
module Task1 where

--import Debug.Trace (traceShowId)

data Point = Point
           { x :: !Int
           , y :: !Int
           }
  deriving (Show, Eq)

plus :: Point -> Point -> Point
plus a b = Point (x a + x b) (y a + y b)

minus :: Point -> Point -> Point
minus a b = Point (x a - x b) (y a - y b)

size :: Point -> Double
size p = sqrt . fromIntegral $ (x p ^ 2) + (y p ^ 2)

scalarProduct :: Point -> Point -> Int
scalarProduct a b = (x a * x b) + (y a * y b)

crossProduct :: Point -> Point -> Int
crossProduct a b = (x a * y b) - (x b * y a)

perimeter :: [Point] -> Double
perimeter []           = 0
perimeter (xx : other) = countPerimeter 0 xx xx other
  where
    countPerimeter :: Double -> Point -> Point -> [Point] -> Double
    countPerimeter !accum first prev (p : other1) = countPerimeter (accum + size (minus prev p)) first p other1
    countPerimeter !accum first prev []           = accum + size (minus first prev)

doubleArea :: [Point] -> Int
doubleArea []          = 0
doubleArea (xx : list) = countDoubleArea 0 xx xx list
  where
    countDoubleArea :: Int -> Point -> Point -> [Point] -> Int
    countDoubleArea !accum first prev (p : other) = countDoubleArea (accum + trapezoidDoubleArea prev p) first p other
    countDoubleArea !accum first prev []          = accum + trapezoidDoubleArea prev first

    trapezoidDoubleArea :: Point -> Point -> Int
    trapezoidDoubleArea p1 p2 = (y p1 + y p2) * (x p2 - x p1)
