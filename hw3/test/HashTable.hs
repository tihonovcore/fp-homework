module HashTable where

import Task2
import Test.Hspec (hspec, describe, it, shouldBe)
import Data.Maybe (fromMaybe)
import Control.Monad.List (void, forM_)
import Control.Concurrent.Async (mapConcurrently)

testHashTable :: IO ()
testHashTable = hspec $
  describe "Task2" $ do
    it "different keys" $ do
      cht <- newCHT :: IO (ConcurrentHashTable String String)
      putCHT "well"   "again" cht
      putCHT "twenty" "five"  cht
      well   <- getCHT "well"   cht
      twenty <- getCHT "twenty" cht

      well   `shouldBe` Just "again"
      twenty `shouldBe` Just "five"

    it "clashed keys" $ do
      cht <- newCHT :: IO (ConcurrentHashTable String String)
      putCHT "putin" "previous president" cht
      beforeClash <- getCHT "putin"       cht
      beforeClash `shouldBe` Just "previous president"

      putCHT "putin" "next president"     cht
      afterClash  <- getCHT "putin"       cht
      afterClash  `shouldBe` Just "next president"

    it "CHT.put <-> CHT.size" $ do
      cht <- newCHT :: IO (ConcurrentHashTable String String)
      s0 <- sizeCHT cht
      s0 `shouldBe` 0

      putCHT "nadeus'" "nawi deistviya" cht
      s1 <- sizeCHT cht
      s1 `shouldBe` 1

      putCHT "po" "dostoinstvy" cht
      s2 <- sizeCHT cht
      s2 `shouldBe` 2

      putCHT "otsenyat" "potomki" cht
      s3 <- sizeCHT cht
      s3 `shouldBe` 3

    it "CHT.put <-> CHT.get" $ do
      cht <- newCHT :: IO (ConcurrentHashTable Int Int)
      putCHT 19 92 cht
      n11 <- getCHT 19   cht
      n11 `shouldBe` Just 92

      putCHT 69 96 cht
      n21 <- getCHT 19   cht
      n21 `shouldBe` Just 92
      n22 <- getCHT 69   cht
      n22 `shouldBe` Just 96

      putCHT 2020 88 cht
      n31 <- getCHT 19   cht
      n31 `shouldBe` Just 92
      n32 <- getCHT 69   cht
      n32 `shouldBe` Just 96
      n33 <- getCHT 2020 cht
      n33 `shouldBe` Just 88

    it "CHT.rehash" $ do
      cht <- newCHT :: IO (ConcurrentHashTable Int Int)

      let nums = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
      forM_ nums (\n -> putCHT n (100 - n) cht)

      sz <- sizeCHT cht
      sz `shouldBe` 10

      values <- mapM (\n -> fromMaybe 0 <$> getCHT n cht) nums
      let keys = map (100 -) values
      let mappingIsOk = foldl (\a (l, r) -> a && l == r) True (zip nums keys)
      mappingIsOk `shouldBe` True

    it "parallel" $ do
      cht <- newCHT :: IO (ConcurrentHashTable Int Int)
      void (mapConcurrently (action cht) [1 .. 100])
      void (mapConcurrently (action cht) [1 .. 100])
      sz <- sizeCHT cht
      sz `shouldBe` 190
      where
        action :: ConcurrentHashTable Int Int -> Int -> IO ()
        action cht n = do
          e <- getCHT n cht
          case e of
            Nothing -> putCHT n (n * n) cht
            Just  _ -> putCHT (n * n) n cht