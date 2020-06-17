{-# LANGUAGE Rank2Types #-}

module Task5 where

import System.Directory (doesFileExist, doesDirectoryExist, getPermissions, readable, getDirectoryContents)
import System.FilePath (takeFileName)
import Control.Monad (filterM)
import Lens.Micro (Lens', lens, Traversal', filtered, traversed, (%~))

data FS
  = Dir
  { _name     :: FilePath  -- название папки, не полный путь
  , _contents :: [FS]
  }
  | File
  { _name     :: FilePath  -- название файла, не полный путь
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
      dirs <- traverse readDirectory dirPaths
      return $ oldDir { _contents = _contents oldDir ++ dirs }

    skipDot :: IO [FilePath] -> IO [FilePath]
    skipDot = fmap $ filter (\s -> show s /= "\".\"" && show s /= "\"..\"")

    toAbsolute :: FilePath -> IO [FilePath] -> IO [FilePath]
    toAbsolute prefix = fmap $ map (\fp -> prefix ++ "/" ++ fp)

name :: Lens' FS FilePath
name = lens _name (\fs new -> fs { _name = new })

contents :: Lens' FS [FS]
contents upd fs@(Dir  _ content) = (\new -> fs {_contents = new}) <$> upd content
contents upd fs@(File _        ) = fs <$ upd []

isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

-- | Get subdirectories
dirs :: Traversal' FS FS
dirs = contents.traversed.filtered isDir

-- | Get subfiles
files :: Traversal' FS FS
files = contents.traversed.filtered (not . isDir)

-- | Get directory name TODO: expected Maybe
--dirName :: Lens' FS FilePath
--dirName upd fs@(File  _) = fs <$ upd ""
--dirName upd fs@(Dir n _) = (\new -> fs {_name = new}) <$> upd n

-- | Get file name if it is File, [] otherwise
fileName :: Lens' FS FilePath
fileName upd fs@(File  n) = (\new -> fs {_name = new}) <$> upd n
fileName upd fs@(Dir _ _) = fs <$ upd ""

-- | Change rootname to "/"


-- | Add suffix to root
--addToRoot :: String -> Lens' FS FS
--addToRoot suffix upd fs@(Dir n _) = (\new -> fs { _name = new }) <$> upd n  

-- | Get name of first directory in content
--first :: Lens' [FS] FS
--first = undefined
--
--firstDir :: Lens' FS FS
--firstDir = dirs.first

-- | Get only file's name from Dir
filesNames :: Traversal' FS FilePath
filesNames = files.name
