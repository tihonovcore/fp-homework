module Task8 where

import ComonadUtils
import Control.Comonad (Comonad(..))
import System.Random (StdGen, newStdGen, random)
import Control.Concurrent (threadDelay)

-- | Representation of human health
data Status = Healthy
            | PassiveInfected
            | ActiveInfected
            | Recovered

instance Show Status where
  show Healthy         = " "
  show PassiveInfected = "?"
  show ActiveInfected  = "#"
  show Recovered       = "@"

-- | Representation of human
data Person = Person
  { status :: Status
  , stdGen :: StdGen
  , daysInStatus :: Int
  }

instance Show Person where
  show p = show $ status p

-- | Update status of selected person
writeStatus :: Status -> Grid Person -> Grid Person
writeStatus s g = flip gridWrite g $ (gridRead g) { status = s }

-- | Creates started Grid with one infected person
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

-- | Functions, that move current position to position
-- of one of neighbours
neighbours :: [Grid Person -> Grid Person]
neighbours = [left, right, up, down]

-- | Count infected neighbours
infectedNeighbours :: Grid Person -> Int
infectedNeighbours g = notHealthyCount $ map (\d -> extract $ d g) neighbours
  where
    notHealthyCount :: [Person] -> Int
    notHealthyCount = length . filter notHealthy

    notHealthy :: Person -> Bool
    notHealthy p = case status p of
                     PassiveInfected -> True
                     ActiveInfected  -> True
                     _               -> False

-- TODO: move as property
infectionProbability :: Double
infectionProbability = 0.07

incubationPeriod :: Int
incubationPeriod = 7

daysWithSymptoms :: Int
daysWithSymptoms = 10

immunityDuration :: Int
immunityDuration = 7  

-- TODO: add trips
-- | Get current person, update his status and returns result
rule :: Grid Person -> Person
rule g = 
  let person = gridRead g in
  case status person of
    Healthy         -> if infectedNeighbours g == 0
                       then person
                       else healthyRule person
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
    healthyRule :: Person -> Person
    healthyRule person =
      let contactsWithInfected = infectedNeighbours g in
      let (newPerson, outcomes) = getNRandomProbabilisticOutcome person contactsWithInfected in
        if any (infectionProbability >) outcomes
        then newPerson { status = PassiveInfected, daysInStatus = 0 }
        else newPerson
    
    getNRandomProbabilisticOutcome :: Person -> Int -> (Person, [Double])
    getNRandomProbabilisticOutcome currPerson n | n <= 0    = (currPerson, [])
                                                | otherwise = 
      let (rv, gen) = (random :: StdGen -> (Double, StdGen)) $ stdGen currPerson in
      let (newPerson, list) = getNRandomProbabilisticOutcome (currPerson { stdGen = gen }) (n - 1) in
        (newPerson, rv : list)

-- | Make one step of infection
step :: Grid Person -> Grid Person
step = extend rule

render :: Show a => Grid a -> String
render = gridShowN 15 . gridShow

runN :: Int -> IO ()
runN n = do
  s <- start
  runImpl 0 s
  where
    runImpl :: Int -> Grid Person -> IO ()
    runImpl k s | k == n    = return ()
                | otherwise = do
      putStrLn $ render s
      threadDelay 200000
      runImpl (k + 1) (step s)
