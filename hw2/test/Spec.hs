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

  describe "append" $ do
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
      let d1 = commit "onaUtanula" d0
      let d2 = VCSCommands.log "onaUtanula" =<< d1
      d2 `shouldBe` Left (FileNotFound "onaUtanula") --TODO: FileNotInVcsError
      --TODO: file in subdir
--    it "file in sub directory" $ do
--      d0 <- startDirectory
--      let d1 = commit "whatHappendWithTheSubmarine" d0
--      let d2 = VCSCommands.log "lala" =<< d1
--      d2 `shouldBe` Left (FileNotFound "whatHappendWithTheSubmarine")

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
      --TODO: file in subdir
--    it "file in subdir" $ do
--      d0 <- startDirectory
--      let d1 = rewriteFile d0 "lala" "дурсли не любили гарри, потому что он крестраж и они более уязвимы к магии, чем его друзья"
--      let d2 = add "lala" =<< d1
--      let d3 = (\d -> append d "lala" "\nого! вот так теория") =<< d2
--      let d4 = commit "lala" =<< d3
--      let d5 = rmFileFromVcs "lala" =<< d4
--      let d6 = VCSCommands.log "lala" =<< d5
--      d6 `shouldBe` Left (FileNotFound "")

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
      --TODO: file in subdir
--    it "file in subdir" $ do
--      d0 <- startDirectory
--      let d1 = rewriteFile d0 "lala" "дурсли не любили гарри, потому что он крестраж и они более уязвимы к магии, чем его друзья"
--      let d2 = add "lala" =<< d1
--      let d3 = (\d -> append d "lala" "\nого! вот так теория") =<< d2
--      let d4 = commit "lala" =<< d3
--      let d5 = rmFileFromVcs "lala" =<< d4
--      let d6 = VCSCommands.log "lala" =<< d5
--      d6 `shouldBe` Left (FileNotFound "")

  describe "vcs composition" $ do
    it "add -> rewrite -> commit -> log" $ do
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "landasd"
      let d2 = add "lala" =<< d1
      let d3 = (\d -> rewriteFile d "lala" "londan") =<< d2
      let d4 = (\d -> commit "lala" d) =<< d3
      let result = (\d -> VCSCommands.log "lala" d) =<< d4
      result `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan"

    it "merge" $ do
      -- VCS default action begin
      d0 <- startDirectory
      let d1 = rewriteFile d0 "lala" "landasd"
      let d2 = add "lala" =<< d1
      let d3 = (\d -> rewriteFile d "lala" "londan") =<< d2
      let d4 = (\d -> commit "lala" d) =<< d3
      -- VCS default action end

      let left  = merge "lala" 1 2 MSLeft  =<< d4
      let leftLog = VCSCommands.log "lala" =<< left
      leftLog `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan\n### 3: \nlandasd"

      let right  = merge "lala" 1 2 MSRight =<< d4
      let rightLog = VCSCommands.log "lala" =<< right
      rightLog  `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan\n### 3: \nlondand"

      let both  = merge "lala" 1 2 MSBoth  =<< d4
      let bothLog = VCSCommands.log "lala" =<< both
      bothLog  `shouldBe` Right "\n### 1: \nlandasd\n### 2: \nlondan\n### 3: \nl\na >>> o\nnda\ns >>> n\nd"

-- TODO: init ??
-- TODO: history
