import Lib
import DirectoryState

import Test.Hspec

startDirectory :: IO Directory
startDirectory = do
  fs <- readDirectoryState "/home/tihonovcore/fp-homework/hw2/testData"
  case fs of (DS _ d) -> return d

catr :: Name -> OpMonad Directory -> OpMonad Data
catr name' = (=<<) (`cat` name')

main :: IO ()
main = hspec $ do
  describe "dir" $
    it "dir" $ do
      d <- startDirectory
      dir d `shouldBe` "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/cats\n| | meow\n| anotherFile\n| notReadMe\n| close" --TODO rm time

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
      fmap dir (rm d "notReadMe") `shouldBe` Right "/home/tihonovcore/fp-homework/hw2/testData\n| /home/tihonovcore/fp-homework/hw2/testData/dogs\n| | woof\n| /home/tihonovcore/fp-homework/hw2/testData/elleFunning\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/elle\n| | | NY\n| | /home/tihonovcore/fp-homework/hw2/testData/elleFunning/funning\n| | | wjuh\n\n| /home/tihonovcore/fp-homework/hw2/testData/cats\n| | meow\n| anotherFile\n| close"--TODO rm time
    it "rmFile not found" $ do
      d <- startDirectory
      fmap dir (rm d "nonExisistsdfs") `shouldBe` Left (Seq (FileNotFound "nonExisistsdfs") (DirNotFound "nonExisistsdfs"))  
    --TODO
--    it "rmNoPermitions" $ do
--      d <- startDirectory
--      fmap dir (rm d "notReadMe") `shouldBe` Right ""
    it "rmDir" $ do
      d <- startDirectory
      fmap dir (rm d "cats") `shouldBe` Right "" --TODO: doesnt work - now path is absolute
    it "rmDir not found" $ do
      d <- startDirectory
      fmap dir (rm d "narwhals") `shouldBe` Left (Seq (FileNotFound "narwhals") (DirNotFound "narwhals"))
   --TODO
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
      catr "notReadMe" (addToFile d "notReadMe" "\nif u younger 18") `shouldBe` Right "lalala u've read! prokaznik\n\nif u younger 18"
    it "file not found" $ do
      d <- startDirectory
      catr "nonExisistsdfs" (addToFile d "nonExisistsdfs" "yeah") `shouldBe` Left (FileNotFound "nonExisistsdfs")
    it "no perm (NOTE: file `testData/close should be readOnly)" $ do  -- TODO: write perm before testing
      d <- startDirectory
      catr "close" (addToFile d "close" "yeah") `shouldBe` Left NoPermissions

--TODO: find file

--TODO: touch
-- Создать файл и проверить путь (должен быть полный)

--TODO: mkdir

--TODO: cd
