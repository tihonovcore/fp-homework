import Block6

import Test.Hspec
import Data.Char
import Control.Applicative (Alternative (..))

main :: IO ()
main = hspec $ do
  describe "Block6.instances" $ do
    it "functor" $ do
      runParser (fmap digitToInt parseDigit) "67" `shouldBe` Just (6, "7")

    it "applicative" $ do
      let functionParser = Parser $ \s -> Just (\x -> x * x, s)
      let argumentParser = parseNumber
      runParser (functionParser <*> argumentParser) "67" `shouldBe` Just (4489, [])

    it "alternative" $ do
      let wrong = parseOpen
      let good  = parseDigit
      runParser (wrong <|> good) "67" `shouldBe` Just ('6', "7")

    it "monad" $ do
      let parse = \s -> runParser parseDigit s
      (parse "361" >>= \(d1, s) -> parse s
                   >>= \(d2, s1) -> Just (digitToInt d1 + (digitToInt d2), s1)) `shouldBe` Just (9, "1")

  describe "Block6.parseOpen" $ do
    it "ok" $ do
      runParser ok "input" `shouldBe` Just ((), "input")

    it "eof success" $ do
      runParser eof "" `shouldBe` Just ((), "")

    it "eof fail" $ do
      runParser eof "(())" `shouldBe` Nothing

    it "satisfy success" $ do
      runParser (satisfy isSpace) " (())" `shouldBe` Just (' ', "(())")

    it "satisfy fail" $ do
      runParser (satisfy (\c -> c == ')')) "(())" `shouldBe` Nothing

    it "element" $ do
      runParser element    "komhc" `shouldBe` Just ('k', "omhc")

    it "stream" $ do
      runParser (stream 4) "hello" `shouldBe` Just ("hell", "o")

  describe "Block6.parseOpen" $ do
    it "open" $ do
      runParser parseOpen "(())" `shouldBe` Just ('(', "())")

    it "not open" $ do
      runParser parseOpen "x"    `shouldBe` Nothing

  describe "Block6.parseClose" $ do
    it "close" $ do
      runParser parseClose ")))" `shouldBe` Just (')', "))")

    it "not close" $ do
      runParser parseClose "dx"  `shouldBe` Nothing

  describe "Block6.parseCBS" $ do
    it "empty" $ do
      let cbs = ""
      fmap (\(result, _) -> show result) (runParser parseCBS cbs) `shouldBe` Just cbs

    it "simple" $ do
      let cbs = "()"
      fmap (\(result, _) -> show result) (runParser parseCBS cbs) `shouldBe` Just cbs

    it "concat" $ do
      let cbs = "()()"
      fmap (\(result, _) -> show result) (runParser parseCBS cbs) `shouldBe` Just cbs

    it "few concat" $ do
      let cbs = "()()()()"
      fmap (\(result, _) -> show result) (runParser parseCBS cbs) `shouldBe` Just cbs

    it "inner cbs" $ do
      let cbs = "(())"
      fmap (\(result, _) -> show result) (runParser parseCBS cbs) `shouldBe` Just cbs

    it "inner concat" $ do
      let cbs = "(()(()))"
      fmap (\(result, _) -> show result) (runParser parseCBS cbs) `shouldBe` Just cbs

    it "not enought close" $ do
      let cbs = "(()"
      runParser parseCBS cbs `shouldBe` Nothing

    it "start with close" $ do
      let cbs = ")(())"
      runParser parseCBS cbs `shouldBe` Nothing

    it "wrong char" $ do
      let cbs = "(())(q)()"
      runParser parseCBS cbs `shouldBe` Nothing

  describe "Block6.check" $ do
    it "first is x" $ do
      runParser (check (\c -> c == 'x')) "xyz" `shouldBe` Just ('x', "xyz")

    it "first is not x" $ do
      runParser (check (\c -> c == 'x')) "zyx" `shouldBe` Nothing

  describe "Block6.parseDigit" $ do
    it "first is digit" $ do
      runParser parseDigit "128" `shouldBe` Just ('1', "28")

    it "first is not digit" $ do
      runParser parseDigit "ax"  `shouldBe` Nothing

  describe "Block6.parseSign" $ do
    it "plus" $ do
      runParser parseSign "+128"  `shouldBe` Just (1, "128")

    it "no sign" $ do
      runParser parseSign "234"   `shouldBe` Just (1, "234")

    it "minus" $ do
      runParser parseSign "-74"   `shouldBe` Just (-1, "74")

    it "wrong input" $ do
      runParser parseSign "hekki" `shouldBe` Nothing

  describe "Block6.parseNumberWithAccum" $ do
      it "plus" $ do
        runParser (parseNumberWithAccum   0) "128"   `shouldBe` Just (128, "")

      it "no sign" $ do
        runParser (parseNumberWithAccum 123) "234"   `shouldBe` Just (123234, "")

      it "wrong input" $ do
        runParser (parseNumberWithAccum   0) "hekki" `shouldBe` Just (0, "hekki")

  describe "Block6.parseNumber" $ do
      it "plus" $ do
        runParser parseNumber "+128"  `shouldBe` Just (128,  "")

      it "no sign" $ do
        runParser parseNumber "234"   `shouldBe` Just (234,  "")

      it "minus" $ do
        runParser parseNumber "-994"  `shouldBe` Just (-994, "")

      it "wrong input" $ do
        runParser parseNumber "hekki" `shouldBe` Nothing

  describe "Block6.skipWs" $ do
      it "skip few" $ do
        skipWs "   word"  `shouldBe` Just ("word")

      it "skip nothing" $ do
        skipWs "234"      `shouldBe` Just ("234")

  describe "Block6.parseComma" $ do
      it "single comma" $ do
        runParser parseComma ",tail"  `shouldBe` Just (',',  "tail")

      it "comma with space" $ do
        runParser parseComma   "    , fasdf"   `shouldBe` Just (',',  " fasdf")

  describe "Block6.parseNumberWS" $ do
      it "few ws" $ do
        runParser parseNumberWS "    128 aas"  `shouldBe` Just (128,  " aas")

  describe "Block6.commaThenNumberParser" $ do
    let parse = \n s -> runParser (commaThenNumberParser n) s
    it "without space" $ do
      parse 1 ",-122 tail"            `shouldBe` Just ([-122],  " tail")

    it "with space" $ do
      parse 1 "  ,  +333other"        `shouldBe` Just ([333],  "other")

    it "fail" $ do
      parse 1 "-994"                  `shouldBe` Nothing

    it "few" $ do
      parse 3 ", 4,   5,   21_other"  `shouldBe` Just ([4, 5, 21], "_other")

  describe "Block6.listParser" $ do
      let parse = \s -> runParser listParser s
      it "without space" $ do
        parse "4,1,2,3,1000 tail"     `shouldBe` Just ([1, 2, 3, 1000],  " tail")

      it "with space" $ do
        parse "   3 , -1, 5, 0 tail"  `shouldBe` Just ([-1, 5, 0],  " tail")

      it "negative size" $ do
        parse "-2"              `shouldBe` Nothing

      it "extra comma" $ do
        parse "3, , 3, 4,3 "    `shouldBe` Nothing

      it "strange symbol" $ do
        parse "2, 1, HOHOHO"    `shouldBe` Nothing

  describe "Block6.listlistParser" $ do
      let parse = \s -> runParser listlistParser s
      it "big test" $ do
        parse "3, 1, -1, 0, 0, 1, 9, 2, -4, -4 tail"  `shouldBe` Just ([[1, -1, 0], [], [9], [-4, -4]],  " tail")

      it "default test" $ do
        parse "2, 1,+10  , 3,5,-7, 2"  `shouldBe` Just ([ [1, 10], [5, -7, 2] ],  "")
