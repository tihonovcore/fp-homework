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
  startDir <- getLine       -- read dir. TODO: read as arg
  content <- readDirectoryState startDir
  loop content

loop :: DirectoryState -> IO ()
loop content = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  command <- getCommand
  let (result, newContent) = evalCommand command content
  showResult result
  loop newContent

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
evalCommand :: Command -> DirectoryState -> (String, DirectoryState)
evalCommand c oldState@(DS vcs d) = match c
  where
    match :: Command -> (String, DirectoryState)
    match Dir                      = (dir d, oldState)
    match (MkDir  dirName)         = handleDir  $ mkdir d dirName
    match (Cd     dirName)         = handleDir  $ cd    d dirName
    match (Rm     objName)         = handleDir  $ rm    d objName
    match (Cat   fileName)         = handleData $ cat   d fileName
    match (Main.Info   objName)    = handleData $ showInfo d objName
    match (Find  fileName)         = handleData $ findFile d fileName
    match (Touch fileName time)    = handleDir  $ touch d fileName time
    match (Write fileName content) = handleDir  $ rewriteFile d fileName content
    match (Add   fileName content) = handleDir  $ addToFile   d fileName content
    match Exit                     = undefined
    match _                        = ("Unexpected input", oldState)

    handleDir :: OpMonad Directory -> (String, DirectoryState)
    handleDir (Left     err) = (show err, oldState)
    handleDir (Right newDir) = ("",  DS vcs newDir)

    handleData :: OpMonad Data -> (String, DirectoryState)
    handleData (Left      err) = (show err, oldState)
    handleData (Right content) = (content,  oldState)

showResult :: String -> IO ()
showResult = putStrLn
