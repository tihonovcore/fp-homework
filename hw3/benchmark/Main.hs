{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main (whnf, bench, bgroup, defaultMain)
import System.Random
import Task1

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

main :: IO ()
main = do
  !points5 <- randomNPoints 100000
  !points6 <- randomNPoints 1000000
  !points7 <- randomNPoints 10000000
  defaultMain [
    bgroup "accumPerimeter" [ bench "1e5" $ whnf perimeter points5
                            , bench "1e6" $ whnf perimeter points6
                            , bench "1e7" $ whnf perimeter points7
                            ]
    ,
    bgroup "accumDoubleArea" [ bench "1e5" $ whnf doubleArea points5
                             , bench "1e6" $ whnf doubleArea points6
                             , bench "1e7" $ whnf doubleArea points7
                             ]
    ]
