module DirectoryState where

import System.Directory
import Control.Monad (filterM)
import System.FilePath

import Utils
import Debug.Trace (trace)

-- TODO: file info: size, etc
-- |File <name> <content>
data File = File String String

instance Show File where
  show (File name con) = name

-- | Info <realName> <isoName> <size> <countFiles> <rights TODO: not int>
data Info = Info String String Int Int Int

-- TODO: show info
instance Show Info where
  show (Info name _ _ _ _) = name

data Directory = Directory Info [Directory] [File]

instance Show Directory where
  show = render ""

getName :: Directory -> String
getName (Directory (Info name _ _ _ _) _ _) = name

render :: String -> Directory -> String
render indent (Directory info subDirs files) =
  let newIndent = indent ++ "| " in
  show info ++ "\n" ++ showWithIndentDir newIndent subDirs ++ showWithIndentFile newIndent files
    where
      showWithIndentFile :: String -> [File] -> String
      showWithIndentFile _      [] = ""
      showWithIndentFile indent (x : xs) = indent ++ show x ++ ln xs ++ showWithIndentFile indent xs

      showWithIndentDir :: String -> [Directory] -> String
      showWithIndentDir _      [] = ""
      showWithIndentDir indent (x : xs) = indent ++ render indent x ++ "\n" ++ showWithIndentDir indent xs

      ln :: [a] -> String
      ln [] = ""
      ln _  = "\n"

-- TODO: check is ok?
data VCSFile = VCSFile String [File]
data VCSDirectory = VCSDirectory String [VCSDirectory] [VCSFile]

-- | <vcsDir> <currDir>
data DirectoryState = DS VCSDirectory Directory

instance Show DirectoryState where
  show (DS _ curr) = show "### VCS: TODO\n" ++ "### CURR:\n" ++ show curr

-- |Make simple Info
newInfo :: String -> Info
newInfo name = Info name ".." 0 0 0

-- |Create new DirectoryState with argument dir as current
newDirectoryState :: Directory -> DirectoryState
newDirectoryState = DS (VCSDirectory "empty" [] [])

-- |Read DirectoryState from file system
-- TODO: clear
readDirectoryState :: String -> IO DirectoryState
readDirectoryState dirName = do
  directory <- readDirectory dirName
  readVCS dirName $ newDirectoryState directory
    where
      readDirectory :: String -> IO Directory
      readDirectory name = do
        pathList <- ((toAbsolute name) . skipDot . getDirectoryContents) name
        d <- readDirs pathList $ Directory (newInfo name) [] []
        readFiles pathList d

      readFiles :: [FilePath] -> Directory -> IO Directory
      readFiles pathList (Directory name dirs _) =
        let filePaths = filterM doesFileExist pathList
            files = (traverse mkFile =<< filePaths)
            newDir = fmap (\filesList -> Directory name dirs filesList) files
        in newDir

      readDirs :: [FilePath] -> Directory -> IO Directory
      readDirs pathList (Directory name _ files) =
        let dirPaths = filterM doesDirectoryExist pathList
            dirs = (traverse readDirectory =<< dirPaths)
            newDir = fmap (\dirsList -> Directory name dirsList files) dirs
        in newDir

      skipDot :: IO [FilePath] -> IO [FilePath]
      skipDot = fmap $ filter (\s -> show s /= "\".\"" && show s /= "\"..\"")

      toAbsolute :: String -> IO [FilePath] -> IO [FilePath]
      toAbsolute prefix = fmap $ map (\fp -> prefix ++ "/" ++ fp)

-- | <path to dir> <state>
-- Add VCSState to State
-- TODO: read VCS
readVCS :: String -> DirectoryState -> IO DirectoryState
readVCS _ = return

mkFile :: FilePath -> IO File
mkFile path = File name <$> content
  where
    name :: FilePath
    name = takeFileName path --TODO: only name

    content :: IO String
    content = readFile path
