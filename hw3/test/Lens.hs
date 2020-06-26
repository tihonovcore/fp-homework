module Lens where

import Task5
import Task6
import Task7
import Test.Hspec (hspec, describe, it, shouldBe)
import Lens.Micro ((^?), (^..))
import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath)

testDir :: FS
testDir = Dir "root" [testSubDirA, testSubDirB, testSubDirC, File "lal.ka"]

testSubDirA :: FS
testSubDirA = Dir "a" []

testSubDirB :: FS
testSubDirB = Dir "b" [File "bbb.let", File "ome.ga"]

testSubDirC :: FS
testSubDirC = Dir "c" [Dir "subd" [], File "subf.ile"]

testLens :: IO ()
testLens = hspec $ do
  describe "Task5" $ do
    it "check directory loader" $ do
      currDirPath <- getCurrentDirectory
      currDir     <- readDirectory (joinPath [currDirPath, "benchmark"])
      show currDir `shouldBe` "DIR: benchmark\n\nFILE: Main.hs"

  describe "Task6" $ do
    it "cd exists" $ testDir ^? cd "b" `shouldBe` Just (Dir "b" [File "bbb.let", File "ome.ga"])

    it "cd non exists" $ testDir ^? cd "d" `shouldBe` Nothing

    it "ls exitsts" $ testDir ^.. cd "c" . ls `shouldBe` ["subd", "subf.ile"]

    it "ls non exitsts" $ testDir ^.. cd "d" . ls `shouldBe` []

    it "file exitsts" $ testDir ^? cd "c" . file "subf.ile" `shouldBe` Just "subf.ile"

    it "file non exitsts" $ testDir ^? cd "c" . file "subd" `shouldBe` Nothing

  describe "Task7" $ do
    it "changeExtension of exitsts file" $ do
      let testDirWithChangedExtension = Dir "root" [testSubDirA, testSubDirB, testSubDirC, File "lal.lyat"]
      changeExtension "lyat" testDir `shouldBe` testDirWithChangedExtension

    it "get names" $ getAllNamesRecursive testDir `shouldBe`  ["lal.ka", "a", "b", "c", "bbb.let", "ome.ga", "subf.ile", "subd"]

    it "remove empty" $ removeIfEmpty "a" testDir `shouldBe` Dir "root" [testSubDirB, testSubDirC, File "lal.ka"]

    it "try to remove non empty" $ removeIfEmpty "b" testDir `shouldBe` testDir

    it "try to remove non exists" $ removeIfEmpty "POT" testDir `shouldBe` testDir
