{-# LANGUAGE Rank2Types #-}

module Task5 where

import System.Directory (doesFileExist, doesDirectoryExist, getPermissions, readable, getDirectoryContents)
import System.FilePath (takeFileName)
import Control.Monad (filterM)
import Lens.Micro (Lens', lens, Traversal', filtered, traversed, (%~), (^.), (&), (.~))

-- | Representation for file system
data FS
  = Dir
  { _name     :: FilePath
  , _contents :: [FS]
  }
  | File
  { _name     :: FilePath
  }
  deriving Eq

instance Show FS where
  show (File n)  = "FILE: " <> n
  show (Dir n c) = "DIR: "  <> n <> "\n" <> foldl (\a fs -> a <> "\n" <> show fs) "" c

-- |Reads file system tree
readDirectory :: FilePath -> IO FS
readDirectory currDirName = do
  permissions <- getPermissions currDirName
  let emptyDir = Dir (takeFileName currDirName) []
  if readable permissions
  then do
    pathList <- (toAbsolute currDirName . skipDot . getDirectoryContents) currDirName
    d <- readDirs pathList emptyDir
    readFiles pathList d
  else return emptyDir
  where
    readFiles :: [FilePath] -> FS -> IO FS
    readFiles pathList oldDir = do
      filePaths <- filterM doesFileExist pathList
      let fileNames = fmap takeFileName filePaths
      return $ oldDir { _contents = _contents oldDir ++ fmap File fileNames }

    readDirs :: [FilePath] -> FS -> IO FS
    readDirs pathList oldDir = do
      dirPaths <- filterM doesDirectoryExist pathList
      dirsList <- traverse readDirectory dirPaths
      return $ oldDir { _contents = _contents oldDir ++ dirsList }

    skipDot :: IO [FilePath] -> IO [FilePath]
    skipDot = fmap $ filter (\s -> show s /= "\".\"" && show s /= "\"..\"")

    toAbsolute :: FilePath -> IO [FilePath] -> IO [FilePath]
    toAbsolute prefix = fmap $ map (\fp -> prefix ++ "/" ++ fp)

-- | Get name of FS object
name :: Lens' FS FilePath
name = lens _name (\fs new -> fs { _name = new })

-- | Get from directory its subdirectories and files
contents :: Lens' FS [FS]
contents upd fs@(Dir  _ content) = (\new -> fs {_contents = new}) <$> upd content
contents upd fs@(File _        ) = fs <$ upd []

-- | Check that FS object is directory
isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

-- | Get subdirectories
dirs :: Traversal' FS FS
dirs = contents.traversed.filtered isDir

-- | Get files from directory
files :: Traversal' FS FS
files = contents.traversed.filtered (not . isDir)

-- | Get file name if it is File, [] otherwise
fileName :: FS -> FilePath
fileName fs = fs ^. files.name

-- | Change root's name to "/"
changeRootName :: FS -> FS
changeRootName fs = fs & dirs.name .~ "/"

-- | Add suffix to root
addToRoot :: String -> FS -> FS
addToRoot suffix fs = fs & dirs.name %~ (++) suffix

-- | Get name of first directory in content
firstDirName :: FS -> FilePath
firstDirName fs = fs ^. dirs.name

-- | Get only file's name from Dir
filesNames :: Traversal' FS FilePath
filesNames = files.name
