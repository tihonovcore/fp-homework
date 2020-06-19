module Task8 where

import ComonadUtils
import Control.Comonad (Comonad(..))
import System.Random (StdGen, newStdGen, random)
import Control.Concurrent (threadDelay)

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

-- TODO: move as property
infectionProbability :: Double
infectionProbability = 0.3

incubationPeriod :: Int
incubationPeriod = 7

daysWithSymptoms :: Int
daysWithSymptoms = 10

immunityDuration :: Int
immunityDuration = 7  

-- TODO: add trips
rule :: Grid Person -> Person
rule g = 
  let person = gridRead g in
  case status person of
    Healthy         -> if infectedNeighbours g == 0 
                       then person 
                       else healthyRule
    PassiveInfected -> if daysInStatus person + 1 == incubationPeriod -- TODO: commonize
                       then person { status = ActiveInfected, daysInStatus = 0 }
                       else person { daysInStatus = daysInStatus person + 1 }
    ActiveInfected  -> if daysInStatus person + 1 == daysWithSymptoms
                       then person { status = Recovered, daysInStatus = 0 }
                       else person { daysInStatus = daysInStatus person + 1 }
    Recovered       -> if daysInStatus person + 1 == immunityDuration
                       then person { status = Healthy, daysInStatus = 0 }
                       else person { daysInStatus = daysInStatus person + 1 }
  where
    healthyRule :: Person
    healthyRule =
      let (rv, gen) = (random :: StdGen -> (Double, StdGen)) $ stdGen (extract g) in
        if rv < 1 - (1 - infectionProbability) * fromIntegral (infectedNeighbours g)
        then Person PassiveInfected gen 0
        else (extract g) { stdGen = gen }

step :: Grid Person -> Grid Person
step = extend rule

render :: Show a => Grid a -> String
render = gridShowN 10 . gridShow

runN :: Int -> IO ()
runN n = do
  s <- start
  runImpl 0 s
  where
    runImpl :: Int -> Grid Person -> IO ()
    runImpl k s | k == n    = return ()
                | otherwise = do
      putStrLn $ render s
      threadDelay 1000000
      runImpl (k + 1) (step s)
