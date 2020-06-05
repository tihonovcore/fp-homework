import Task1

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
      size (Point 3 4) `shouldBe` 5
      size (Point 6 8) `shouldBe` 10

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
