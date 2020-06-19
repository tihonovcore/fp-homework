module Task8 where

import ComonadUtils
import Control.Comonad (Comonad(..))
import System.Random (StdGen, newStdGen)

data Status = Healthy
            | PassiveInfected
            | ActiveInfected
            | Recovered

instance Show Status where
  show Healthy         = " "
  show PassiveInfected = "?"
  show ActiveInfected  = "#"
  show Recovered       = "@"

data Person = Person
  { status :: Status
  , stdGen :: StdGen
  , daysInStatus :: Int
  }

instance Show Person where
  show p = show $ status p

writeStatus :: Status -> Grid Person -> Grid Person
writeStatus s g = flip gridWrite g $ (gridRead g) { status = s }

start :: IO (Grid Person)
start = writeStatus PassiveInfected . Grid <$> (genericMove <$> healthy <*> healthy <*> healthyLZ)
  where
    healthy :: IO (ListZipper Person -> ListZipper Person)
    healthy = const <$> healthyLZ

    healthyLZ :: IO (ListZipper Person)
    healthyLZ = LZ <$> healthyList <*> healthyPerson <*> healthyList

    healthyList :: IO [Person]
    healthyList = do
      hp <- healthyPerson
      return $ iterate (const hp) hp

    healthyPerson :: IO Person
    healthyPerson = do
      g <- newStdGen
      return $ Person Healthy g 0

neighbours :: [Grid Person -> Grid Person]
neighbours = [left, right, up, down]

infectedNeighbours :: Grid Person -> Int
infectedNeighbours g = notHealthyCount $ map (\d -> extract $ d g) neighbours
  where
    notHealthyCount :: [Person] -> Int
    notHealthyCount = length . filter notHealthy

    notHealthy :: Person -> Bool
    notHealthy p = case status p of
                     Healthy -> False
                     _       -> True

rule :: Grid Person -> Person
rule g = case infectedNeighbours g of
           0 -> extract g
           _ -> (extract g) { status = PassiveInfected } --TODO use rnd

step :: Grid Person -> Grid Person
step = extend rule

nStep :: Int -> Grid Person -> Grid Person
nStep n g | n <= 0    = g
          | otherwise = nStep (n - 1) (step g)

render :: Show a => Grid a -> String
render = gridShowN 4 . gridShow

runN :: Int -> IO ()
runN n = do
  s <- start
  runImpl 0 s
  where
    runImpl :: Int -> Grid Person -> IO ()
    runImpl k s | k == n    = return ()
                | otherwise = do
      putStrLn $ render s
      runImpl (k + 1) (step s)
