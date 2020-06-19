module Task8 where

import ComonadUtils
import Control.Comonad (Comonad(..))

data Person = Healthy | Infected | Recovered

instance Show Person where
  show Healthy   = " "
  show Infected  = "#"
  show Recovered = "@"

start :: Grid Person
start = gridWrite Infected $ Grid (genericMove healthy healthy healthyLZ)
  where
    healthy :: ListZipper Person -> ListZipper Person
    healthy = const healthyLZ

    healthyLZ :: ListZipper Person
    healthyLZ = LZ healthyList Healthy healthyList

    healthyList :: [Person]
    healthyList = iterate (const Healthy) Healthy

neighbours :: [Grid Person -> Grid Person]
neighbours = [left, right, up, down]

infectedNeighbours :: Grid Person -> Int
infectedNeighbours g = notHealthyCount $ map (\d -> extract $ d g) neighbours
  where
    notHealthyCount :: [Person] -> Int
    notHealthyCount = length . filter notHealthy

    notHealthy :: Person -> Bool
    notHealthy p = case p of
                     Healthy -> False
                     _       -> True

rule :: Grid Person -> Person
rule g = case infectedNeighbours g of
           0 -> extract g
           _ -> Infected

step :: Grid Person -> Grid Person
step = extend rule

nStep :: Int -> Grid Person -> Grid Person
nStep n g | n <= 0    = g
          | otherwise = nStep (n - 1) (step g)

render :: Show a => Grid a -> String
render = gridShowN 4 . gridShow
