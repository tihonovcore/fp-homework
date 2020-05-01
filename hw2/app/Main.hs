module Main where

import Lib
import DirectoryState
import System.IO (hSetBuffering, stdout, BufferMode(..))

path :: String
path = "/home/tihonovcore/testHaskellCL"

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
             | Touch String
             | Cd    String
             | Exit
             | Error

getCommand :: IO Command
getCommand = do
  line <- getLine
  case line of s | s == "dir"    -> return Dir
                 | s == "mkdir"  -> fmap MkDir getLine
                 | s == "touch"  -> fmap Touch getLine
                 | s == "cd"     -> fmap Cd    getLine
                 | s == "exit"   -> return Exit
                 | otherwise     -> return Error

-- TODO: work with vcs
evalCommand :: Command -> DirectoryState -> (String, DirectoryState)
evalCommand c oldState@(DS vcs d) = match c
  where
    match :: Command -> (String, DirectoryState)
    match Dir              = (dir d, oldState)
    match (MkDir  dirName) = handle "Directory already exists" $ mkdir d dirName
    match (Cd     dirName) = handle "Directory not found"      $ cd    d dirName
    match (Touch fileName) = handle "File already exists"      $ touch d fileName
    match Exit  = undefined
    match _     = handle "Unexpected input" Nothing

    handle :: String -> Maybe Directory -> (String, DirectoryState)
    handle _       (Just dd) = ("", DS vcs dd)
    handle message Nothing   = (message, oldState)

showResult :: String -> IO ()
showResult = putStrLn
