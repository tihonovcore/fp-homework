import Lib
import DirectoryState

import Test.Hspec

startDirectory :: IO Directory
startDirectory = do
  fs <- readDirectoryState "/home/tihonovcore/fp-homework/hw2/testData"
  case fs of (DS _ d) -> return d

catr :: Name -> Maybe Directory -> Maybe Data
catr name' = (=<<) (`cat` name')

main :: IO ()
main = hspec $ do
  describe "dir" $
    it "dir" $ do
      d <- startDirectory
      dir d `shouldBe` "" --TODO rm time

  describe "cat" $ do
    it "cat" $ do
      d <- startDirectory
      cat d "notReadMe"   `shouldBe` Just "lalala u've read! prokaznik\n"
    it "cat" $ do
      d <- startDirectory
      cat d "anotherFile" `shouldBe` Just "there is one more 123456789!@#$%^&*()"
    it "cat" $ do
      d <- startDirectory
      cat d "fail"        `shouldBe` Nothing
    -- TODO: no permissions

  describe "rm" $ do
    it "rmFile" $ do
      d <- startDirectory
      fmap dir (rm d "notReadMe") `shouldBe` Just ""--TODO rm time
    it "rmFile not found" $ do
      d <- startDirectory
      fmap dir (rm d "nonExisistsdfs") `shouldBe` Nothing
    --TODO
--    it "rmNoPermitions" $ do
--      d <- startDirectory
--      fmap dir (rm d "notReadMe") `shouldBe` Just ""
    it "rmDir" $ do
      d <- startDirectory
      fmap dir (rm d "cats") `shouldBe` Just "" --TODO: doesnt work - now path is absolute
    it "rmDir not found" $ do
      d <- startDirectory
      fmap dir (rm d "narwhals") `shouldBe` Nothing
   --TODO
--    it "rmDir no perm" $ do
--      d <- startDirectory
--      fmap dir (rm d "cats") `shouldBe` Just ""

  describe "showInfo" $ do
    it "file" $ do
      d <- startDirectory
      showInfo d "notReadMe" `shouldBe` Just "" --TODO rm time

    it "file not found" $ do
      d <- startDirectory
      showInfo d "nonExisistsdfs" `shouldBe` Nothing
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
      catr "notReadMe" (rewriteFile d "notReadMe" "oh, STOP READING") `shouldBe` Just "oh, STOP READING"
    it "file not found" $ do
      d <- startDirectory
      catr "nonExisistsdfs" (rewriteFile d "nonExisistsdfs" "lalalalal") `shouldBe` Nothing
    --TODO
--    it "no perm" $ do
--      d <- startDirectory
--      catr "nonExisistsdfs" (rewriteFile d "nonExisistsdfs" "lalalalal") `shouldBe` Nothing

  describe "addToFile" $ do
    it "succ" $ do
      d <- startDirectory
      --TODO use current time to check changes
      catr "notReadMe" (addToFile d "notReadMe" "\nif u younger 18") `shouldBe` Just "lalala u've read! prokaznik\n\nif u younger 18"
    it "file not found" $ do
      d <- startDirectory
      catr "nonExisistsdfs" (addToFile d "nonExisistsdfs" "yeah") `shouldBe` Nothing
    --TODO
--    it "no perm" $ do
--      d <- startDirectory
      catr "nonExisistsdfs" (addToFile d "nonExisistsdfs" "yeah") `shouldBe` Nothing

--TODO: find file

--TODO: touch

--TODO: mkdir

--TODO: cd
