import Task1
import Task2

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.List (forM_, void)
import Data.Maybe (fromMaybe)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Task1.operations" $ do
    it "plus" $ do
      let p1 = Point 3 2
      let p2 = Point 7 6
      plus p1 p2 `shouldBe` Point 10 8

    it "minus" $ do
      let p1 = Point 1 3
      let p2 = Point 3 4
      minus p2 p1 `shouldBe` Point 2 1

    it "size" $ do
      Task1.size (Point 3 4) `shouldBe` 5
      Task1.size (Point 6 8) `shouldBe` 10

    it "scalarProduct" $ do
      let p1 = Point 3 5
      let p2 = Point 4 6
      scalarProduct p1 p2 `shouldBe` 42

    it "crossProduct" $ do
      let p1 = Point 3 5
      let p2 = Point 4 6
      crossProduct p1 p2 `shouldBe` -2

    it "perimeter" $ do
      let xs = [1, 2, 2, 3, 3, 4, 4, 1] :: [Int]
      let ys = [1, 1, 2, 2, 1, 1, 3, 3] :: [Int]
      let p  = fmap (\(l, r) -> Point l r) (zip xs ys)
      perimeter p `shouldBe` 12.0

    it "doubleArea" $ do
      let xs = [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1, 0] :: [Int]
      let ys = [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 2, 2, 3, 3, 4, 4, 3, 3, 2, 2] :: [Int]
      let p  = fmap (\(l, r) -> Point l r) (zip xs ys)
      doubleArea p `shouldBe` 24

    it "perimeter of strange figure" $ do
      let xs = [1, 3, 5, 5, 4, 5, 6, 7, 8, 9, 5, 6, 3] :: [Int]
      let ys = [1, 2, 1, 2, 4, 3, 1, 4, 2, 4, 6, 4, 5] :: [Int]
      let p  = fmap (\(l, r) -> Point l r) (zip xs ys)
      
      let eps = 0.000001
      (perimeter p - 33.3355166352) < eps `shouldBe` True

    it "doubleArea of strange figure" $ do
      let xs = [1, 3, 5, 5, 4, 5, 6, 7, 8, 9, 5, 6, 3] :: [Int]
      let ys = [1, 2, 1, 2, 4, 3, 1, 4, 2, 4, 6, 4, 5] :: [Int]
      let p  = fmap (\(l, r) -> Point l r) (zip xs ys)
      doubleArea p `shouldBe` 35

  describe "Task2.sequetially" $ do
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
