import Lib
import VCSCommands
import DirectoryState
import System.Directory (setPermissions, emptyPermissions, getCurrentDirectory)

import Test.Hspec
import Data.Time (getCurrentTime)
import System.FilePath (joinPath)

currDir :: IO String
currDir = do
  filePath <- getCurrentDirectory
  return $ joinPath [filePath, "testData"]

startDirectory :: IO Directory
startDirectory = do
  currDirPath <- currDir
  setPermissions (joinPath [currDirPath, "close"])    emptyPermissions
  setPermissions (joinPath [currDirPath, "closeDir"]) emptyPermissions
  readDirectoryState currDirPath

catr :: Name -> OpMonad Directory -> OpMonad Data
catr name' = (=<<) (`cat` name')

dirr :: OpMonad Directory -> OpMonad Data
dirr curr = dir <$> curr

main :: IO ()
main = hspec $ do
  describe "dir" $
    it "dir" $ do
      d <- startDirectory
      dir d `shouldBe` "\9500\9472 closeDir\n\9500\9472 dogs\n\9500\9472 elleFunning\n\9500\9472 cats\n\9500\9472 anotherFile\n\9500\9472 lala\n\9500\9472 notReadMe\n\9492\9472 close"

  describe "cat" $ do
    it "succ" $ do
      d <- startDirectory
      cat d "notReadMe"   `shouldBe` Right "lalala u've read! prokaznik\n"
    it "succ too" $ do
      d <- startDirectory
      cat d "anotherFile" `shouldBe` Right "there is one more 123456789!@#$%^&*()"
    it "file not found" $ do
      d <- startDirectory
      cat d "fail"        `shouldBe` Left (FileNotFound "fail")
    it "no pemissions" $ do
      d <- startDirectory
      cat d "close"       `shouldBe` Left NoPermissions

  describe "rm" $ do
    it "succ" $ do
      d <- startDirectory
      fmap dir (rm d "notReadMe") `shouldBe` Right "\9500\9472 closeDir\n\9500\9472 dogs\n\9500\9472 elleFunning\n\9500\9472 cats\n\9500\9472 lala\n\9500\9472 anotherFile\n\9492\9472 close"
    it "file not found" $ do
      d <- startDirectory
      fmap dir (rm d "nonExisistsdfs") `shouldBe` Left (Seq (FileNotFound "nonExisistsdfs") (DirNotFound "nonExisistsdfs"))
    it "file no permitions" $ do
      d <- startDirectory
      fmap dir (rm d "close") `shouldBe` Left (Seq NoPermissions (DirNotFound "close"))
    it "directory" $ do
      d <- startDirectory
      fmap dir (rm d "cats") `shouldBe` Right "\9500\9472 elleFunning\n\9500\9472 dogs\n\9500\9472 closeDir\n\9500\9472 anotherFile\n\9500\9472 lala\n\9500\9472 notReadMe\n\9492\9472 close"
    it "directory not found" $ do
      d <- startDirectory
      fmap dir (rm d "narwhals") `shouldBe` Left (Seq (FileNotFound "narwhals") (DirNotFound "narwhals"))
    it "directory no permissions" $ do
      d <- startDirectory
      fmap dir (rm d "closeDir") `shouldBe` Left (Seq (FileNotFound "closeDir") NoPermissions)

  describe "showInfo" $ do
    it "file" $ do
      d <- startDirectory
      currDirPath <- currDir
      (unwords . init . lines <$> showInfo d "notReadMe") `shouldBe` Right ("Object \"notReadMe\" at " ++ currDirPath ++ " Size: 28 Permissions {readable = True, writable = True, executable = False, searchable = False}")

    it "file not found" $ do
      d <- startDirectory
      showInfo d "nonExisistsdfs" `shouldBe` Left (Seq (FileNotFound "nonExisistsdfs") (DirNotFound "nonExisistsdfs"))
    
    it "directory" $ do
      d <- startDirectory
      currDirPath <- currDir
      showInfo d "cats" `shouldBe` Right ("Object \"cats\" at " ++ currDirPath ++ "\nSize: 0\nPermissions {readable = True, writable = True, executable = False, searchable = True}\nCount files: 0")

  describe "rewriteFile" $ do
    it "succ" $ do
      d <- startDirectory
      catr "notReadMe" (rewriteFile d "notReadMe" "oh, STOP READING") `shouldBe` Right "oh, STOP READING"
    it "file not found" $ do
      d <- startDirectory
      catr "nonExisistsdfs" (rewriteFile d "nonExisistsdfs" "lalalalal") `shouldBe` Left (FileNotFound "nonExisistsdfs")
    it "no permission" $ do
      d <- startDirectory
      catr "close" (rewriteFile d "close" "lalalalal") `shouldBe` Left NoPermissions

  describe "append" $ do
    it "succ" $ do
      d <- startDirectory
      catr "notReadMe" (append d "notReadMe" "\nif u younger 18") `shouldBe` Right "lalala u've read! prokaznik\n\nif u younger 18"
    it "file not found" $ do
      d <- startDirectory
      catr "nonExisistsdfs" (append d "nonExisistsdfs" "yeah") `shouldBe` Left (FileNotFound "nonExisistsdfs")
    it "no permission" $ do
      d <- startDirectory
      catr "close" (append d "close" "yeah") `shouldBe` Left NoPermissions

  describe "findFile" $ do
    it "in current dir" $ do
      d <- startDirectory
      currDirPath <- currDir
      findFile d "close" `shouldBe` Right (joinPath [currDirPath, "close"])
    it "sub dir" $ do
      d <- startDirectory
      currDirPath <- currDir
      findFile d "NY" `shouldBe` Right (joinPath [currDirPath, "elleFunning/elle/NY"])
    it "file not found" $ do
      d <- startDirectory
      findFile d "NYNYA" `shouldBe` Left (ObjectNotFound "NYNYA")

  describe "touch" $ do
    it "create file" $ do
      d <- startDirectory
      time <- getCurrentTime
      dirr (touch d "alka-lalka" time) `shouldBe` Right "\9500\9472 closeDir\n\9500\9472 dogs\n\9500\9472 elleFunning\n\9500\9472 cats\n\9500\9472 alka-lalka\n\9500\9472 anotherFile\n\9500\9472 lala\n\9500\9472 notReadMe\n\9492\9472 close"
    it "check path & name" $ do
      d <- startDirectory
      time <- getCurrentTime
      currDirPath <- currDir
      let action = Right . unwords . init . lines =<< (flip showInfo "alka-lalka" =<< touch d "alka-lalka" time)
      action `shouldBe` Right ("Object \"alka-lalka\" at " ++ currDirPath ++ " Size: 0 Permissions {readable = True, writable = True, executable = False, searchable = False}")
    it "file already exisits" $ do
      d <- startDirectory
      time <- getCurrentTime
      dirr (touch d "lala" time) `shouldBe` Left (FileAlreadyExists "lala")
    it "dir already exisits" $ do
      d <- startDirectory
      time <- getCurrentTime
      dirr (touch d "dogs" time) `shouldBe` Left (DirAlreadyExists "dogs")

  describe "cd" $ do
    it "succ" $ do
      d <- startDirectory
      dirr (cd d "cats") `shouldBe` Right "\9500\9472 ..\n\9492\9472 meow"
    it ".." $ do
      d0 <- startDirectory
      let d1 = cd d0 "elleFunning"
      let res = (\d -> cd d "..") =<< d1
      dirr res `shouldBe` Right "\9500\9472 elleFunning\n\9500\9472 dogs\n\9500\9472 closeDir\n\9500\9472 cats\n\9500\9472 anotherFile\n\9500\9472 lala\n\9500\9472 notReadMe\n\9492\9472 close"
    it "directory not found" $ do
      d <- startDirectory
      dirr (cd d "dooogs") `shouldBe` Left (DirNotFound "dooogs")

  describe "mkdir" $ do
    it "success" $ do
      d <- startDirectory
      dirr (mkdir d "trees") `shouldBe` Right "\9500\9472 trees\n\9500\9472 closeDir\n\9500\9472 dogs\n\9500\9472 elleFunning\n\9500\9472 cats\n\9500\9472 anotherFile\n\9500\9472 lala\n\9500\9472 notReadMe\n\9492\9472 close"
    it "dir already Exists" $ do
      d <- startDirectory
      dirr (mkdir d "cats") `shouldBe` Left (DirAlreadyExists "cats")

  describe "add & log" $ do
    it "add: ok, log: ok" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "test331"
      let d2 = add "lala" =<< d1
      let d3 = VCSCommands.log "lala" =<< d2
      d3 `shouldBe` Right "\n### 1: \ntest331"
    it "add: fail, log: ok" $ do
      d0 <- startDirectory
      let d1 = add "banditka" d0
      let d2 = VCSCommands.log "lala" =<< d1
      d2 `shouldBe` Left (FileNotFound "banditka")
    it "add: ok, log: fail" $ do
      d0 <- startDirectory
      let d1 = VCSCommands.log "tatata" d0
      d1 `shouldBe` Left (FileNotFound "tatata")

  describe "commit" $ do
    it "success" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "tessa"
      let d2 = add "lala" =<< d1
      let d3 = (\d -> append d "lala" " russian forward") =<< d2
      let d4 = commit "lala" =<< d3
      let d5 = VCSCommands.log "lala" =<< d4
      d5 `shouldBe` Right "\n### 1: \ntessa\n### 2: \ntessa russian forward"
    it "file not found" $ do
      d0 <- startDirectory
      let d1 = commit "whatHappendWithTheSubmarine" d0
      let d2 = VCSCommands.log "whatHappendWithTheSubmarine" =<< d1
      d2 `shouldBe` Left (FileNotFound "whatHappendWithTheSubmarine")
    it "file not in vcs" $ do
      d0 <- startDirectory
      let d1 = commit "notReadMe" d0
      let d2 = VCSCommands.log "notReadMe" =<< d1
      d2 `shouldBe` Left (FileNotInVcs "notReadMe")

  describe "remove file from vcs" $ do
    it "success" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "дурсли не любили гарри, потому что он крестраж и они более уязвимы к магии, чем его друзья"
      let d2 = add "lala" =<< d1
      let d3 = (\d -> append d "lala" "\nого! вот так теория") =<< d2
      let d4 = commit "lala" =<< d3
      let d5 = rmFileFromVcs "lala" =<< d4
      let d6 = VCSCommands.log "lala" =<< d5
      d6 `shouldBe` Left (FileNotInVcs "lala")
    it "file not found" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "europe" "make make grate again"
      let d2 = add "europe" =<< d1
      let d3 = (\d -> append d "europe" "\nstop using make") =<< d2
      let d4 = commit "europe" =<< d3
      let d5 = rmFileFromVcs "europe" =<< d4
      let d6 = VCSCommands.log "europe" =<< d5
      d6 `shouldBe` Left (FileNotFound "europe")

  describe "show revisions" $ do
    it "success" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "anotherFile" "3rd may 2020. 7025 dead brazillians. their president: * happy *"
      let d2 = add "anotherFile" =<< d1
      let d3 = (\d -> append d "anotherFile" "adfsdf") =<< d2
      let d4 = commit "anotherFile" =<< d3
      let d5 = showRevision "anotherFile" 1 =<< d4
      d5 `shouldBe` Right "3rd may 2020. 7025 dead brazillians. their president: * happy *"
    it "file not found" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "melissaTea" "как перестать пить чай и начать пить"
      let d2 = add "melissaTea" =<< d1
      let d3 = (\d -> append d "melissaTea" "\n и начать пить") =<< d2
      let d4 = commit "melissaTea" =<< d3
      let d5 = showRevision "melissaTea" 2 =<< d4
      d5 `shouldBe` Left (FileNotFound "melissaTea")
    it "too big" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "anotherFile" "errrrrorororo"
      let d2 = add "anotherFile" =<< d1
      let d3 = (\d -> append d "anotherFile" "nana") =<< d2
      let d4 = commit "anotherFile" =<< d3
      let d5 = showRevision "anotherFile" 1345 =<< d4
      d5 `shouldBe` Left (WrongRevisionIndex "anotherFile")
    it "too small" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "anotherFile" "some text"
      let d2 = add "anotherFile" =<< d1
      let d3 = (\d -> append d "anotherFile" "\nлашадь") =<< d2
      let d4 = commit "anotherFile" =<< d3
      let d5 = showRevision "anotherFile" (-133) =<< d4
      d5 `shouldBe` Left (WrongRevisionIndex "anotherFile")

  describe "vcs composition" $ do
    it "add -> rewrite -> commit -> log" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "landasd"
      let d2 = add "lala" =<< d1
      let d3 = (\d -> rewriteFile d "lala" "londan") =<< d2
      let d4 = commit "lala" =<< d3
      let result = VCSCommands.log "lala" =<< d4
      result `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan"

    it "merge" $ do
      -- VCS default action begin
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "landasd"
      let d2 = add "lala" =<< d1
      let d3 = (\d -> rewriteFile d "lala" "londan") =<< d2
      let d4 = commit "lala" =<< d3
      -- VCS default action end

      let left  = merge "lala" 1 2 MSLeft  =<< d4
      let leftLog = VCSCommands.log "lala" =<< left
      leftLog `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan\n### 3: \nlandasd"

      let right  = merge "lala" 1 2 MSRight =<< d4
      let rightLog = VCSCommands.log "lala" =<< right
      rightLog  `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan\n### 3: \nlondan"

      let both  = merge "lala" 1 2 MSBoth  =<< d4
      let bothLog = VCSCommands.log "lala" =<< both
      bothLog  `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan\n### 3: \nl\na >>> o\nnda\nsd >>> n\n"

    it "merge: add/remove content" $ do
      -- VCS default action begin
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "hello"
      let d2 = add "lala" =<< d1
      let d3 = (\d -> rewriteFile d "lala" "hello world") =<< d2
      let d4 = commit "lala" =<< d3
      -- VCS default action end

      let left  = merge "lala" 1 2 MSLeft  =<< d4
      let leftLog = VCSCommands.log "lala" =<< left
      leftLog `shouldBe` Right "\n### 1: \nhello\n### 2: \nhello world\n### 3: \nhello world"

      let right  = merge "lala" 1 2 MSRight =<< d4
      let rightLog = VCSCommands.log "lala" =<< right
      rightLog  `shouldBe` Right "\n### 1: \nhello\n### 2: \nhello world\n### 3: \nhello world"

      let both  = merge "lala" 1 2 MSBoth  =<< d4
      let bothLog = VCSCommands.log "lala" =<< both
      bothLog  `shouldBe` Right "\n### 1: \nhello\n### 2: \nhello world\n### 3: \nhello world"
