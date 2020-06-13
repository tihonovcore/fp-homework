{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main (whnf, bench, bgroup, defaultMain)
import System.Random
import Task1
import Task2

main :: IO ()
main = do
  runTask1Benchmark
  runTask2Benchmark
  -- TODO: task 2 speed up & add parallel becnhmark

runTask1Benchmark :: IO ()
runTask1Benchmark = do
  !points5 <- randomNPoints 100000
  !points6 <- randomNPoints 1000000
  !points7 <- randomNPoints 10000000
  defaultMain [
    bgroup "accumPerimeter" [ bench "1e5" $ whnf perimeter points5        -- 2.49 ms
                            , bench "1e6" $ whnf perimeter points6        -- 15.2 ms
                            , bench "1e7" $ whnf perimeter points7        -- 99.8 ms
                            ]
    ,
    bgroup "accumDoubleArea" [ bench "1e5" $ whnf doubleArea points5      -- 1.24 ms
                             , bench "1e6" $ whnf doubleArea points6      -- 10.1 ms
                             , bench "1e7" $ whnf doubleArea points7      -- 67.6 ms
                             ]
    ,
    bgroup "naivePerimeter" [ bench "1e5" $ whnf naivePerimeter points5   -- 6.78 ms
                            , bench "1e6" $ whnf naivePerimeter points6   -- 94   ms
                            , bench "1e7" $ whnf naivePerimeter points7   -- 770  ms
                            ]
    ,
    bgroup "naiveDoubleArea" [ bench "1e5" $ whnf naiveDoubleArea points5 -- 5.6  ms
                             , bench "1e6" $ whnf naiveDoubleArea points6 -- 71.6 ms
                             , bench "1e7" $ whnf naiveDoubleArea points7 -- 816  ms
                             ]
    ]

randomNPoints :: Int -> IO [Point]
randomNPoints npoints = randomNPointsImpl npoints =<< newStdGen
  where
    randomNPointsImpl :: Int -> StdGen -> IO [Point]
    randomNPointsImpl n g | n <= 0    = return []
                          | otherwise = do
      let border = 100000
      let (xx, g') = randomR (-border, border) g
      let (yy, _ ) = randomR (-border, border) g'
      fmap ((:) (Point xx yy)) (randomNPointsImpl (n - 1) g')

runTask2Benchmark :: IO ()
runTask2Benchmark = do
  !cht5    <- newCHT :: IO (ConcurrentHashTable Int Int)
  !cht6    <- newCHT :: IO (ConcurrentHashTable Int Int)
  !cht7    <- newCHT :: IO (ConcurrentHashTable Int Int)
  !keys5   <- randomNNumbers (-100000) 100000 100000
  !keys6   <- randomNNumbers (-100000) 100000 1000000
  !keys7   <- randomNNumbers (-100000) 100000 10000000
  !values5 <- randomNNumbers (-100000) 100000 100000
  !values6 <- randomNNumbers (-100000) 100000 1000000
  !values7 <- randomNNumbers (-100000) 100000 10000000
  !operations5 <- randomNNumbers 0 2 100000
  !operations6 <- randomNNumbers 0 2 1000000
  !operations7 <- randomNNumbers 0 2 10000000
  defaultMain [
    bgroup "CHT" [ bench "1e5" $ whnf (action cht5) (zip3 keys5 values5 operations5) -- 8.819 ns
                 , bench "1e6" $ whnf (action cht6) (zip3 keys6 values6 operations6) -- 9.343 ns
                 , bench "1e7" $ whnf (action cht7) (zip3 keys7 values7 operations7) -- 9.677 ns
                 ]
    ]
    where
      action :: ConcurrentHashTable Int Int -> [(Int, Int, Int)] -> IO ()
      action _ [] = return ()
      action cht ((key, value, op) : xs) | op == 0   = getCHT key cht       >> action cht xs
                                         | op == 1   = putCHT key value cht >> action cht xs
                                         | otherwise = sizeCHT cht          >> action cht xs

randomNNumbers :: Int -> Int -> Int -> IO [Int]
randomNNumbers from to nnumbers = randomNNumbersImpl nnumbers =<< newStdGen
  where
    randomNNumbersImpl :: Int -> StdGen -> IO [Int]
    randomNNumbersImpl n g | n <= 0 = return []
                           | otherwise = do
      let (curr, g') = randomR (from, to) g
      (:) curr <$> randomNNumbersImpl (n - 1) g'
