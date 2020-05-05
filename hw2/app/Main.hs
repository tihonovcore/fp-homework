module Main where

import Lib
import VCSCommands
import DirectoryState
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Time.Clock (UTCTime)
import Data.Time (getCurrentTime)
import Numeric (readDec)

testPath :: String
--testPath = "/home/tihonovcore/testHaskellCL"
testPath = "/home/tihonovcore/fp-homework/hw2/testData"

main :: IO ()
main = do
--  startDir <- getLine       -- read dir. TODO: read as arg
  let startDir = testPath
  directory <- readDirectoryState startDir
  loop directory

loop :: Directory -> IO ()
loop directory = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  command <- getCommand
  case evalCommand command directory of
    Just (result, newContent) -> do
      showResult result
      loop newContent
    Nothing -> do
      writeDirectoryState directory
      return ()

data Command = Dir
             | MkDir  Name
             | Touch  Name UTCTime
             | Cd     Name
             | Rm     Name
             | Cat    Name
             | Info   Name
             | Write  Name Data
             | Append Name Data
             | Find   Name
             | VCSCommit Name
             | VCSAdd    Name
             | VCSLog    Name
             | VCSRevision Name Int
             | VCSRmRevision Name Int
             | VCSRmFile Name
             | VCSMerge Name Int Int MergeStrategy
             | Exit
             | Error

getCommand :: IO Command
getCommand = do
  line <- getLine
  case line of s | s == "dir"    -> return Dir
                 | s == "mkdir"  -> fmap MkDir getLine
                 | s == "rm"     -> fmap Rm    getLine
                 | s == "cat"    -> fmap Cat   getLine
                 | s == "find"   -> fmap Find  getLine
                 | s == "cd"     -> fmap Cd    getLine
                 | s == "info"   -> fmap Main.Info getLine
                 | s == "write"  -> Write  <$> getLine <*> getLine
                 | s == "append" -> Append <$> getLine <*> getLine
                 | s == "touch"  -> Touch  <$> getLine <*> getCurrentTime
                 | s == "add"    -> fmap VCSAdd    getLine
                 | s == "log"    -> fmap VCSLog    getLine
                 | s == "commit" -> fmap VCSCommit getLine
                 | s == "showRev" -> VCSRevision   <$> getLine <*> readNumber
                 | s == "rmRev"   -> VCSRmRevision <$> getLine <*> readNumber
                 | s == "vcs-rmFile" -> VCSRmFile <$> getLine
                 | s == "vcs-merge" -> VCSMerge <$> getLine <*> readNumber <*> readNumber <*> readStrategy
                 | s == "exit"   -> return Exit
                 | otherwise     -> return Error

readNumber :: IO Int
readNumber = do
  list <- readDec <$> getLine
  case list of
    []    -> error "parse errror"
    x : _ -> return $ fst x

readStrategy :: IO MergeStrategy
readStrategy = do
  line <- getLine
  return $ if line == "l" 
           then MSLeft
           else if line == "r"
           then MSRight
           else if line == "b"
           then MSBoth
           else MSInteractive

-- TODO: work with vcs
evalCommand :: Command -> Directory -> Maybe (String, Directory)
evalCommand c d = match c
  where
    match :: Command -> Maybe (String, Directory)
    match Dir                      = Just (dir d, d)
    match (MkDir  dirName)         = handleDir  $ mkdir d dirName
    match (Cd     dirName)         = handleDir  $ cd    d dirName
    match (Rm     objName)         = handleDir  $ rm    d objName
    match (Cat   fileName)         = handleData $ cat   d fileName
    match (Main.Info objName)      = handleData $ showInfo d objName
    match (Find  fileName)         = handleData $ findFile d fileName
    match (Touch fileName time)    = handleDir  $ touch d fileName time
    match (Write  fileName input)  = handleDir  $ rewriteFile d fileName input
    match (Append fileName input)  = handleDir  $ append   d fileName input
    match (VCSLog fileName)                       = handleData $ VCSCommands.log fileName d
    match (VCSCommit fileName)                    = handleDir  $ commit          fileName d
    match (VCSAdd    fileName)                    = handleDir  $ VCSCommands.add fileName d
    match (VCSRmFile fileName)                    = handleDir  $ rmFileFromVcs   fileName d
    match (VCSRevision   fileName index)          = handleData $ showRevision    fileName index d
    match (VCSRmRevision fileName index)          = handleDir  $ removeRevision  fileName index d
    match (VCSMerge fileName left right strategy) = handleDir  $ merge           fileName left right strategy d
    match Exit                     = Nothing
    match _                        = Just ("Unexpected input", d)

    handleDir :: OpMonad Directory -> Maybe (String, Directory)
    handleDir (Left     err) = Just (show err, d)
    handleDir (Right newDir) = Just ("",  newDir)

    handleData :: OpMonad Data -> Maybe (String, Directory)
    handleData (Left    err) = Just (show err, d)
    handleData (Right input) = Just (input,    d)

showResult :: String -> IO ()
showResult = putStrLn
