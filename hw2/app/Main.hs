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
    match Dir                   = (dir d, oldState)
    match (MkDir  dirName)      = handleDir  "Directory already exists" $ mkdir d dirName
    match (Cd     dirName)      = handleDir  "Directory not found"      $ cd    d dirName
    match (Rm     objName)      = handleDir  "Object not found"         $ rm    d objName
    match (Cat   fileName)      = handleData "File not found"           $ cat   d fileName
    match (Main.Info   objName) = handleData "Object not found"         $ showInfo d objName
--    match (Find  fileName)      = handleData "File not found"           $ findFile d fileName
    match (Touch fileName time) = handleDir  "File already exists"      $ touch d fileName time
    match (Write fileName content) = handleDir "File not found"         $ rewriteFile d fileName content
    match (Add   fileName content) = handleDir "File not found"         $ addToFile   d fileName content
    match Exit  = undefined
    match _     = handleDir "Unexpected input" Nothing

    handleDir :: String -> Maybe Directory -> (String, DirectoryState)
    handleDir _       (Just dd) = ("", DS vcs dd)
    handleDir message Nothing   = (message, oldState)
    
    handleData :: String -> Maybe Data -> (String, DirectoryState)
    handleData _   (Just msg) = (msg, oldState)
    handleData err Nothing    = (err, oldState)

showResult :: String -> IO ()
showResult = putStrLn
