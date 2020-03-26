import Block1
import Block2
import Block3
import Block4
import Block5

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Block1.nextDay" $ do
    it "After Monday" $ do
      nextDay Monday `shouldBe` Tuesday

    it "After Wednesday" $ do
      nextDay Wednesday `shouldBe` Thursday

    it "After Sundey" $ do
      nextDay Sunday `shouldNotBe` Thursday

    it "After Friyday" $ do
      nextDay Friday `shouldNotBe` Thursday

  describe "Block1.afterDays" $ do
    it "Zero days" $ do
      afterDays Tuesday 0 `shouldBe` Tuesday

    it "Few days" $ do
      afterDays Monday 5 `shouldBe` Saturday

    it "A lot days" $ do
      afterDays Monday 1089 `shouldBe` Friday

    it "After negate days" $ do
      afterDays Sunday (-9) `shouldBe` Friday

  describe "Block1.isWeekend" $ do
    it "Weekend" $ do
      isWeekend Saturday `shouldBe` True

    it "Not weekend" $ do
      isWeekend Tuesday `shouldBe` False

  describe "Block1.daysToParty" $ do
    it "Today" $ do
      daysToParty Friday`shouldBe` 0

    it "Few days" $ do
      daysToParty Monday `shouldBe` 4

  describe "Block1.plus" $ do
    it "3 + 2 = 5" $ do
      plus (S $ S $ S Z) (S $ S Z) `shouldBe` (S $ S $ S $ S $ S Z)

    it "0 + 2 = 2" $ do
      plus Z (S $ S Z) `shouldBe` (S $ S Z)

  describe "Block1.mult" $ do
    it "3 * 2 = 6" $ do
      mult (S $ S $ S Z) (S $ S Z) `shouldBe` (S $ S $ S $ S $ S $ S Z)

    it "4 * 0 = 0" $ do
      mult (S $ S $ S $ S Z) Z `shouldBe` Z

  describe "Block1.subs" $ do
    it "4 - 2 = 2" $ do
      subs (S $ S $ S $ S Z) (S $ S Z) `shouldBe` (S $ S Z)

    it "3 - 5 = 0" $ do
      subs (S $ S $ S Z) (S $ S $ S $ S $ S Z) `shouldBe` Z

  describe "Block1.fromIntToNat" $ do
    it "fromIntToNat 5" $ do
      fromIntToNat 5 `shouldBe` (S $ S $ S $ S $ S Z)

    it "fromIntToNat -2" $ do
      (fromIntToNat (-2)) `shouldBe` Z

  describe "Block1.isEven" $ do
    it "4 isEven" $ do
      isEven (S $ S $ S $ S Z) `shouldBe` True

    it "3 isEven" $ do
      isEven (S $ S $ S Z) `shouldBe` False

  describe "Block1.natDiv" $ do
    it "4 / 2 = 2" $ do
      natDiv (S $ S $ S $ S Z) (S $ S Z) `shouldBe` (S $ S Z)

    it "5 / 3 = 1" $ do
      natDiv (S $ S $ S $ S $ S Z) (S $ S $ S Z) `shouldBe` (S Z)

  describe "Block1.natMod" $ do
    it "3 % 2 = 1" $ do
      natMod (S $ S $ S Z) (S $ S Z) `shouldBe` (S Z)

    it "4 % 2 = 1" $ do
      natMod (S $ S $ S $ S Z) (S $ S Z) `shouldBe` Z

  describe "Block1.isEmpty" $ do
    it "Empty tree" $ do
      isEmpty Leaf `shouldBe` True

    it "Non empty tree" $ do
      isEmpty (Node Leaf (Node Leaf Leaf (Values 3 [])) (Values 5 [])) `shouldBe` False

  describe "Block1.size" $ do
    it "Empty tree" $ do
      size Leaf `shouldBe` 0

    it "Non empty tree" $ do
      size (Node Leaf (Node Leaf Leaf (Values 3 [])) (Values 5 [])) `shouldBe` 2

    it "Non empty tree with value repetition" $ do
      size (Node Leaf (Node Leaf Leaf (Values 3 [3, 3, 3])) (Values 5 [])) `shouldBe` 5

  describe "Block1.find" $ do
    it "Empty tree" $ do
      find 2 Leaf `shouldBe` False

    it "Non empty tree" $ do
      find 7 (Node (Node Leaf Leaf (Values 3 [])) Leaf (Values 5 [])) `shouldBe` False

    it "Non empty tree" $ do
      find 3 (Node (Node Leaf Leaf (Values 3 [])) Leaf (Values 5 [])) `shouldBe` True

    it "Non empty tree with value repetition" $ do
      find 3 (Node (Node Leaf Leaf (Values 3 [3, 3, 3])) Leaf (Values 5 [])) `shouldBe` True

  describe "Block1.put" $ do
    it "Empty tree" $ do
      put (2 :: Int) Leaf `shouldBe` (Node Leaf Leaf (Values 2 []))

    it "Non empty tree" $ do
      let from = (Node (Node Leaf Leaf (Values 3 [])) Leaf (Values 5 []))
          to   = (Node (Node Leaf Leaf (Values 3 [])) (Node Leaf Leaf (Values 7 [])) (Values 5 [])) in
        put (7 :: Int) from `shouldBe` to

    it "Non empty tree" $ do
      let from = (Node Leaf (Node Leaf Leaf (Values 6 [])) (Values 4 []))
          to   = (Node Leaf (Node (Node Leaf Leaf (Values 5 [])) Leaf (Values 6 [])) (Values 4 [])) in
        put (5 :: Int) from `shouldBe` to

    it "Non empty tree with value repetition" $ do
      let from = (Node (Node Leaf Leaf (Values 3 [3, 3, 3])) Leaf (Values 5 []))
          to   = (Node (Node Leaf Leaf (Values 3 [3, 3, 3])) Leaf (Values 5 [5])) in
        put (5 :: Int) from `shouldBe` to

  describe "Block1.fromList" $ do
    it "Empty tree" $ do
      fromList ([] :: [Int]) `shouldBe` Leaf

    it "Non empty tree" $ do
      fromList [4, 5, 2, 5] `shouldBe` (Node (Node Leaf Leaf (Values 2 [])) (Node Leaf Leaf (Values 5 [5])) (Values 4 []))

    it "Tree LRL" $ do
      fromList [6, 9, 7, 8] `shouldBe` (Node Leaf (Node (Node Leaf (Node Leaf Leaf (Values 8 [])) (Values 7 [])) Leaf (Values 9 [])) (Values 6 []))

  describe "Block1.remove" $ do
    it "Remove from empty tree" $ do
      remove 401 Leaf `shouldBe` Leaf

    it "Remove with repetition" $ do
      remove 5 (Node (Node Leaf Leaf (Values 2 [])) (Node Leaf Leaf (Values 5 [5])) (Values 4 [])) `shouldBe` (Node (Node Leaf Leaf (Values 2 [])) (Node Leaf Leaf (Values 5 [])) (Values 4 []))

    it "Remove single" $ do
      let from = (Node Leaf (Node (Node Leaf (Node Leaf Leaf (Values 8 [])) (Values 7 [])) Leaf (Values 9 [])) (Values 6 []))
          to   = (Node Leaf (Node (Node Leaf Leaf                           (Values 7 [])) Leaf (Values 9 [])) (Values 6 [])) 
      remove 8 from `shouldBe` to

  describe "Block2.splitOn" $ do
    it "Split nothing" $ do
      splitOn '/' "" `shouldBe` [""]

    it "Split numbers" $ do
      splitOn 3 [2, 3, 2, 3, 8] `shouldBe` [[2], [2], [8]]

    it "Slash" $ do
      splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]

    it "Dot dot" $ do
      splitOn '.' "hell..owo..rd" `shouldBe` ["hell", "", "owo", "", "rd"]

  describe "Block2.FTree" $ do
    it "Sum" $ do
      foldl (\acc e -> e + acc) 0 (FNode (FLeaf 1) (FNode (FLeaf 21) (FLeaf 4) 0) 0) `shouldBe` 26

    it "Concat" $ do
      foldl (\acc e -> acc ++ (show e)) "" (FNode (FLeaf 1) (FNode (FLeaf 21) (FLeaf 4) 0) 0) `shouldBe` "102104"

  describe "Block3.maybeConcat" $ do
    it "[Integer]" $ do
      maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5]

    it "Chars" $ do
      maybeConcat [Just ['H', 'e'], Nothing, Just ['l'], Just ['p']] `shouldBe` "Help"

  describe "Block3.NonEmpty" $ do
    it "Single" $ do
      (3 :| []) <> (4 :| [5, 6]) `shouldBe` (3 :| [4, 5, 6])

    it "Three lists" $ do
      (1 :| [1, 1]) <> (2 :| [2, 2]) <> (3 :| [3, 3]) `shouldBe` (1 :| [1, 1, 2, 2, 2, 3, 3, 3])

  describe "Block3.Name" $ do
    it "Single" $ do
      Empty <> Name "Sta" <> Empty <> Name "liN" `shouldBe` (Name "Sta.liN")

    it "Mempty" $ do
      Name "Kil " <> mempty <> Name "lme" `shouldBe` (Name "Kil .lme")

  describe "Block4.stringSum" $ do
    it "Weird string" $ do
      stringSum "[Just [1,2,3], Nothing, Just [4,5]]" `shouldBe` Nothing

    it "Good string" $ do
      stringSum "1 2 4    5 5  6 67 7  " `shouldBe` (Just 97)

  describe "Block4.eval" $ do
    it "Good expression" $ do
      let t = Const 2
          f = Const 5 in
        eval (Multiply t (Plus (Subtract f t) $ Divide f t)) `shouldBe` (Right 10)

    it "Expression with error" $ do
          let t = Const 2
              f = Const 5 in
            eval (Divide t (Subtract (Plus t t) $ Pow t t)) `shouldBe` (Left DivisionByZero)
