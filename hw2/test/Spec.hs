import Lib
import VCSCommands
import DirectoryState

import Test.Hspec
import Data.Time (getCurrentTime)

startDirectory :: IO Directory
startDirectory = readDirectoryState "/home/tihonovcore/fp-homework/hw2/testData"

catr :: Name -> OpMonad Directory -> OpMonad Data
catr name' = (=<<) (`cat` name')

dirr :: OpMonad Directory -> OpMonad Data
dirr curr = dir <$> curr

main :: IO ()
main = hspec $ do
  describe "dir" $
    it "dir" $ do
      d <- startDirectory
      dir d `shouldBe` "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/cats\n| | meow\n| anotherFile\n| lala\n| notReadMe\n| close" --TODO rm time

  describe "cat" $ do
    it "cat" $ do
      d <- startDirectory
      cat d "notReadMe"   `shouldBe` Right "lalala u've read! prokaznik\n"
    it "cat" $ do
      d <- startDirectory
      cat d "anotherFile" `shouldBe` Right "there is one more 123456789!@#$%^&*()"
    it "cat" $ do
      d <- startDirectory
      cat d "fail"        `shouldBe` Left (FileNotFound "fail")
    -- TODO: no permissions

  describe "rm" $ do
    it "rmFile" $ do
      d <- startDirectory
      fmap dir (rm d "notReadMe") `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/cats\n| | meow\n| lala\n| anotherFile\n| close"--TODO rm time
    it "rmFile not found" $ do
      d <- startDirectory
      fmap dir (rm d "nonExisistsdfs") `shouldBe` Left (Seq (FileNotFound "nonExisistsdfs") (DirNotFound "nonExisistsdfs"))  
    --TODO: rm file no perm
--    it "rmNoPermitions" $ do
--      d <- startDirectory
--      fmap dir (rm d "notReadMe") `shouldBe` Right ""
    it "rmDir" $ do
      d <- startDirectory
      fmap dir (rm d "cats") `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| anotherFile\n| lala\n| notReadMe\n| close"
    it "rmDir not found" $ do
      d <- startDirectory
      fmap dir (rm d "narwhals") `shouldBe` Left (Seq (FileNotFound "narwhals") (DirNotFound "narwhals"))
   --TODO: rm dir no perm
--    it "rmDir no perm" $ do
--      d <- startDirectory
--      fmap dir (rm d "cats") `shouldBe` Right ""

  describe "showInfo" $ do
    it "file" $ do
      d <- startDirectory
      showInfo d "notReadMe" `shouldBe` Right "How to test? Here's time.."

    it "file not found" $ do
      d <- startDirectory
      showInfo d "nonExisistsdfs" `shouldBe` Left (FileNotFound "nonExisistsdfs")
   --TODO
--    it "dir" $ do
--      d <- startDirectory
--      showInfo d "nonExisistsdfs" `shouldBe` Nothing
--
--    it "dir not found" $ do
--      d <- startDirectory
--      showInfo d "nonExisistsdfs" `shouldBe` Nothing

  describe "rewriteFile" $ do
    it "succ" $ do
      d <- startDirectory
      --TODO use current time to check changes
      catr "notReadMe" (rewriteFile d "notReadMe" "oh, STOP READING") `shouldBe` Right "oh, STOP READING"
    it "file not found" $ do
      d <- startDirectory
      catr "nonExisistsdfs" (rewriteFile d "nonExisistsdfs" "lalalalal") `shouldBe` Left (FileNotFound "nonExisistsdfs")
    it "no perm (NOTE: file `testData/close should be readOnly)" $ do  -- TODO: write perm before testing
      d <- startDirectory
      catr "close" (rewriteFile d "close" "lalalalal") `shouldBe` Left NoPermissions

  describe "addToFile" $ do
    it "succ" $ do
      d <- startDirectory
      --TODO use current time to check changes
      catr "notReadMe" (append d "notReadMe" "\nif u younger 18") `shouldBe` Right "lalala u've read! prokaznik\n\nif u younger 18"
    it "file not found" $ do
      d <- startDirectory
      catr "nonExisistsdfs" (append d "nonExisistsdfs" "yeah") `shouldBe` Left (FileNotFound "nonExisistsdfs")
    it "no perm (NOTE: file `testData/close should be readOnly)" $ do  -- TODO: write perm before testing
      d <- startDirectory
      catr "close" (append d "close" "yeah") `shouldBe` Left NoPermissions

  describe "findFile" $ do
    it "this dir" $ do
      d <- startDirectory
      findFile d "close" `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData/close"
    it "sub dir" $ do
      d <- startDirectory
      findFile d "NY" `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle/NY"
    it "file not found" $ do  -- TODO: write perm before testing
      d <- startDirectory
      findFile d "NYNYA" `shouldBe` Left (ObjectNotFound "NYNYA")

  describe "touch" $ do
    it "create file" $ do
      d <- startDirectory
      time <- getCurrentTime
      dirr (touch d "alka-lalka" time) `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/cats\n| | meow\n| alka-lalka\n| anotherFile\n| lala\n| notReadMe\n| close"
    it "check path & name" $ do
      d <- startDirectory
      time <- getCurrentTime
      (flip showInfo "alka-lalka" =<< touch d "alka-lalka" time) `shouldBe` Right "" -- TODO: how to test? here's currTime
    -- TODO: create in subdir (e.g. `touch /dogs/muha)
    it "file already exisits" $ do
      d <- startDirectory
      time <- getCurrentTime
      dirr (touch d "lala" time) `shouldBe` Left (FileAlreadyExists "lala")
    it "dir already exisits" $ do
      d <- startDirectory
      time <- getCurrentTime
      dirr (touch d "dogs" time) `shouldBe` Left (DirAlreadyExists "dogs")

  describe "cd" $ do
    it "cd to exists" $ do
      d <- startDirectory
      dirr (cd d "cats") `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData/cats\n| /home/tihonovcore/fp-homework/hw2/..\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | | NY\n| | | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | | wjuh\n\n| | /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | | woof\n| | anotherFile\n| | lala\n| | notReadMe\n| | close\n| meow"
    it "cd .." $ do
      d0 <- startDirectory
      let d1 = cd d0 "elleFunning"
      let res = (\d -> cd d "..") =<< d1
      dirr res `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| /home/tihonovcore/fp-homework/hw2/testData/cats\n| | meow\n| anotherFile\n| lala\n| notReadMe\n| close"
    -- TODO: cd to subdir (e.g. `cd /dogs/cuteDogs)
    it "dir not found" $ do
      d <- startDirectory
      dirr (cd d "dooogs") `shouldBe` Left (DirNotFound "dooogs")

  describe "mkdir" $ do
    it "success" $ do
      d <- startDirectory
      dirr (mkdir d "trees") `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/trees\n\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/cats\n| | meow\n| anotherFile\n| lala\n| notReadMe\n| close"
    -- TODO: cd to subdir (e.g. `cd /dogs/cuteDogs)
    it "dir already Exists" $ do
      d <- startDirectory
      dirr (mkdir d "cats") `shouldBe` Left (DirAlreadyExists "cats")

--TODO: find file

--TODO: touch
-- Создать файл и проверить путь (должен быть полный)

--TODO: mkdir

--TODO: cd
