module Main where

import Control.Monad.State.Lazy
--import Debug.Trace --TODO: rm
import Lib
import System.Directory
--import Control.Exception (SomeException, catch)

path :: String
path = "/home/tihonovcore/testHaskellCL"

-- /home/tihonovcore/testHaskellCL
main :: IO ()
main = do
  startDir <- getLine       -- read dir. TODO: read as arg
  content <- readDirState startDir
  loop content

loop :: Directory -> IO ()
loop content = do
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

evalCommand :: Command -> Directory -> (String, Directory)
evalCommand c d = match c
  where
    match :: Command -> (String, Directory)
    match Dir   = (dir d, d)
    match (MkDir  dirName) = handle d "Directory already exists" $ mkdir d dirName
    match (Cd     dirName) = handle d "Directory not found"      $ cd    d dirName
    match (Touch fileName) = handle d "File already exists"      $ touch d fileName
    match Exit  = undefined
    match _     = handle d "Unexpected input" Nothing

    handle :: Directory -> String -> Maybe Directory -> (String, Directory)
    handle _        _       (Just dd) = ("", dd)
    handle oldState message Nothing  = (message, oldState)
    
showResult :: String -> IO ()
showResult = putStrLn

-- TODO: think about errors
-- TODO: rm stateT IO
readDirState :: String -> IO Directory
readDirState dirName = do
  content <- (toAbsolute . skipDot . getDirectoryContents) dirName
  (dirWithFiles, _) <- runStateT (readFiles $ Directory dirName [] []) content
  (dirWithFilesAndDirs, _) <- runStateT (readDirs dirWithFiles) content
  return dirWithFilesAndDirs
  where
    readFiles :: Directory -> StateT [FilePath] IO Directory
    readFiles (Directory name dirs _) =
      StateT $ \pathList ->
        let filePaths = filterM doesFileExist pathList
            files = (traverse mkFile =<< filePaths)
            newDir = fmap (Directory name dirs) files
         in fmap (\d -> (d, pathList)) newDir

    readDirs :: Directory -> StateT [FilePath] IO Directory
    readDirs (Directory name _ files) =
      StateT $ \pathList ->
        let dirPaths = filterM doesDirectoryExist pathList
            dirs = (traverse readDirState =<< dirPaths)
            newDir = fmap (\dirsList -> Directory name dirsList files) dirs
         in fmap (\d -> (d, pathList)) newDir

    skipDot :: IO [FilePath] -> IO [FilePath]
    skipDot = fmap $ filter (\s -> show s /= "\".\"" && show s /= "\"..\"")

    toAbsolute :: IO [FilePath] -> IO [FilePath]
    toAbsolute = fmap $ map (\fp -> dirName ++ "/" ++ fp)
