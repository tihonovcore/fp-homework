module Geometry where

import Task1
import Test.Hspec (hspec, describe, it, shouldBe)

testGeometry :: IO ()
testGeometry = hspec $
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
