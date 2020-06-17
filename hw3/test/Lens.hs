module Lens where

import Task5
import Task6
--import Task7
import Test.Hspec (hspec, describe, it, shouldBe)
import Lens.Micro ((^?), (^..))

testDir :: FS
testDir = Dir "root" [Dir "a" [], Dir "b" [File "bbb"], Dir "c" [Dir "subd" [], File "subf"]]

testLens :: IO ()
testLens = hspec $ do
  describe "Task5" $ do
-- TODO: read dir
-- TODO: simple test for all func
    return ()

  describe "Task6" $ do
    it "cd exists" $ testDir ^? cd "b" `shouldBe` Just (Dir "b" [File "bbb"])

    it "cd non exists" $ testDir ^? cd "d" `shouldBe` Nothing

    it "ls exitsts" $ testDir ^.. cd "c" . ls `shouldBe` ["subd", "subf"]

    it "ls non exitsts" $ testDir ^.. cd "d" . ls `shouldBe` []

    it "file exitsts" $ testDir ^? cd "c" . file "subf" `shouldBe` Just "subf"

    it "file non exitsts" $ testDir ^? cd "c" . file "subd" `shouldBe` Nothing
