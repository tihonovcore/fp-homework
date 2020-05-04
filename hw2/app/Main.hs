module Main where

import Lib
import DirectoryState
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Time.Clock (UTCTime)
import Data.Time (getCurrentTime)

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
             | MkDir String
             | Touch String UTCTime
             | Cd    String
             | Rm    String
             | Cat   String
             | Info  String
             | Write String Data
             | Add   String Data
             | Find  String
             | Exit
             | Error

getCommand :: IO Command
getCommand = do
  line <- getLine
  case line of s | s == "dir"    -> return Dir
                 | s == "mkdir"  -> fmap MkDir getLine
                 | s == "rm"     -> fmap Rm getLine
                 | s == "cat"    -> fmap Cat getLine
                 | s == "info"   -> fmap Main.Info getLine
                 | s == "write"  -> Write <$> getLine <*> getLine
                 | s == "add"    -> Add   <$> getLine <*> getLine
                 | s == "find"   -> fmap Find getLine
                 | s == "touch"  -> Touch <$> getLine <*> getCurrentTime
                 | s == "cd"     -> fmap Cd    getLine
                 | s == "exit"   -> return Exit
                 | otherwise     -> return Error

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
    match (Main.Info   objName)    = handleData $ showInfo d objName
    match (Find  fileName)         = handleData $ findFile d fileName
    match (Touch fileName time)    = handleDir  $ touch d fileName time
    match (Write fileName input) = handleDir  $ rewriteFile d fileName input
    match (Add   fileName input) = handleDir  $ addToFile   d fileName input
--    match (VCSLog fileName)        = handleDataVCS $ VCSCommands.log fileName oldState
--    match (VCSCommit fileName)     = handleDirVCS  $ commit fileName oldState
--    match (VCSAdd    fileName)     = handleDirVCS  $ add    fileName oldState
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
