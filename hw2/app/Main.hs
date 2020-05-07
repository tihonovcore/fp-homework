module Main where

import Lib
import VCSCommands
import VcsIO
import DirectoryState
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Data.Time.Clock (UTCTime)
import Data.Time (getCurrentTime)
import Numeric (readDec)
import System.FilePath (takeDirectory, takeFileName, splitDirectories, joinPath)

testPath :: String
--testPath = "/home/tihonovcore/testHaskellCL"
testPath = "/home/tihonovcore/fp-homework/hw2/testData"

main :: IO ()
main = do
--  startDir <- getLine       -- read dir. TODO: read as arg
  let startDir = testPath
  ddd <- readDirectoryState startDir
  -- TODO: remove absolute
  vcs <- readVcsDirectory "/home/tihonovcore/fp-homework/hw2/vcs/testData"
  loop $ mergeDirAndVcs ddd vcs

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
      let liftedDirectory = liftDirectory directory
      writeDirectoryState liftedDirectory
      writeVcsState liftedDirectory
      return ()

liftDirectory :: Directory -> Directory
liftDirectory directory = 
  case cd directory ".." of
    Left  _ -> directory
    Right r -> liftDirectory r

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

-- TODO: make better
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

evalCommand :: Command -> Directory -> Maybe (String, Directory)
evalCommand c d = match c
  where
    match :: Command -> Maybe (String, Directory)
    match Dir                      = Just (dir d, d)
    match (MkDir  dirName)         = handleDir mkdir dirName
    match (Cd     dirName)         = handleCd dirName
    match (Rm     objName)         = handleDir  rm  objName
    match (Cat   fileName)         = handleData cat fileName  --   d fileName
    match (Main.Info objName)      = handleData showInfo objName
    match (Find  fileName)         = handleData findFile fileName
    match (Touch fileName time)    = handleDir  (swap312 touch time) fileName
    match (Write  fileName input)  = handleDir  (swap312 rewriteFile input) fileName
    match (Append fileName input)  = handleDir  (swap312 append input) fileName
    match (VCSLog fileName)                       = handleData (flip VCSCommands.log) fileName
    match (VCSCommit fileName)                    = handleDir (flip commit)          fileName
    match (VCSAdd    fileName)                    = handleDir (flip VCSCommands.add) fileName
    match (VCSRmFile fileName)                    = handleDir  (flip rmFileFromVcs)   fileName
    match (VCSRevision   fileName index)          = handleData (swap231 showRevision   index)      fileName
    match (VCSRmRevision fileName index)          = handleDir  (swap231 removeRevision index)      fileName
    match (VCSMerge fileName left right strategy) = handleDir  (swap23451 merge left right strategy) fileName
    match Exit                     = Nothing
    match _                        = Just ("Unexpected input", d)
    
    handleCd :: FilePath -> Maybe (String, Directory)
    handleCd filePath = 
      case multiCd filePath d of
        (Left     err) -> Just (show err, d)
        (Right newDir) -> Just ("",  newDir)
    
    swap231 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
    swap231 f = \b c' a -> f a b c'
    
    swap312 :: (a -> b -> c -> d) -> (c -> a -> b -> d)
    swap312 f = \c' a b -> f a b c'
    
    swap23451 :: (a -> b -> c -> d -> e -> f) -> (b -> c -> d -> e -> a -> f)
    swap23451 f b c' d' e a = f a b c' d' e

    handleData :: (Directory -> FilePath -> OpMonad Data) -> FilePath -> Maybe (String, Directory)
    handleData action longPath =
      case flip action (takeFileName longPath) =<< multiCd (takeDirectory longPath) d of
        (Left    err) -> Just (show err, d)
        (Right input) -> Just (input,    d)

    handleDir :: (Directory -> FilePath -> OpMonad Directory) -> FilePath -> Maybe (String, Directory)
    handleDir action longPath =
      case up =<< flip action (takeFileName longPath) =<< multiCd (takeDirectory longPath) d of
        (Left     err) -> Just (show err, d)
        (Right newDir) -> Just ("",  newDir)
        where
          up :: Directory -> OpMonad Directory
          up currDir =
            let upSteps = map (const "..") $ tail $ splitDirectories (takeDirectory longPath) in
            multiCd (joinPath upSteps) currDir


--    handleDir :: OpMonad Directory -> Maybe (String, Directory)
--    handleDir (Left     err) = Just (show err, d)
--    handleDir (Right newDir) = Just ("",  newDir)
--
--    handleData :: OpMonad Data -> Maybe (String, Directory)
--    handleData (Left    err) = Just (show err, d)
--    handleData (Right input) = Just (input,    d)

showResult :: String -> IO ()
showResult = putStrLn
