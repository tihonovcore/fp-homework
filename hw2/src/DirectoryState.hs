module DirectoryState where

import System.Directory
import Control.Monad (filterM)
import System.FilePath
import Data.Time

import Utils
import Debug.Trace (trace)
import System.Directory (Permissions)
import System.Directory.Internal (Permissions)

data CommonInfo = Info
  { path :: String
  , name :: String
  , size :: Integer
  , perm :: Permissions
  }

-- | DirInfo CommonInfo <isoName> <countFiles>
data DirInfo = DirInfo CommonInfo Name Int

instance Show CommonInfo where
  show info = "File \"" ++ name info ++ "\" at " ++ path info ++
    "\nFile size: " ++ show (size info) ++ "\n" ++ show (perm info)

instance Show DirInfo where
  show (DirInfo  common _ countFiles) = show common ++ "\nCount files: " ++ show countFiles

-- | FileInfo CommonInfo <content> <lastAccess>
data File = File
  { commonInfo :: CommonInfo
  , content :: Data
  , revisions :: [Data] 
  , lastAccess :: UTCTime
  }

instance Show File where
  show = name . commonInfo 
--  show (File common _ lastAccess) = show common ++ "\nLast access: " ++ show lastAccess

data Directory = Directory
  { dirInfo  :: DirInfo
  , subDirs  :: [Directory]
  , subFiles :: [File]
  }

instance Show Directory where
  show = render ""

fullName :: CommonInfo -> Name
fullName info = joinPath [path info, name info]

getFileName :: File -> Name
getFileName = name . commonInfo

getFullFileName :: File -> Name
getFullFileName = fullName . commonInfo

getDirName :: Directory -> Name
getDirName (Directory (DirInfo ci _ _) _ _) = name ci

getFullDirName :: Directory -> Name
getFullDirName (Directory (DirInfo ci _ _) _ _) = fullName ci

dirCommonInfo :: Directory -> CommonInfo
dirCommonInfo (Directory (DirInfo ci _ _) _ _) = ci

-- TODO: print DirInfo too
render :: String -> Directory -> String
render indent currDir =
  let newIndent = indent ++ "| "
      dirs  = subDirs  currDir
      files = subFiles currDir in
  getFullDirName currDir ++ "\n" ++ showWithIndentDir newIndent dirs ++ showWithIndentFile newIndent files
    where
      showWithIndentFile :: String -> [File] -> String
      showWithIndentFile _      [] = ""
      showWithIndentFile currIndent (x : xs) = currIndent ++ show x ++ ln xs ++ showWithIndentFile currIndent xs

      showWithIndentDir :: String -> [Directory] -> String
      showWithIndentDir _      [] = ""
      showWithIndentDir currIndent (x : xs) = currIndent ++ render currIndent x ++ "\n" ++ showWithIndentDir currIndent xs

      ln :: [a] -> String
      ln [] = ""
      ln _  = "\n"

emptyInfo :: FilePath -> Name -> CommonInfo
emptyInfo newPath newName =
  let newPerm = setOwnerReadable True $ setOwnerWritable True emptyPermissions in
    Info { path = newPath, name = newName,  size = 0, perm = newPerm }

-- |Make simple Info
newDirInfo :: FilePath -> Name -> DirInfo
newDirInfo newPath newName = 
  DirInfo (emptyInfo newPath newName) ".." 0

-- |Read DirectoryState from file system
-- TODO: clear
readDirectoryState :: Name -> IO Directory
readDirectoryState dirName = do
  directory <- readDirectory dirName
  readVCS dirName directory
    where
      readDirectory :: Name -> IO Directory
      readDirectory currDirName = do
        pathList <- (toAbsolute currDirName . skipDot . getDirectoryContents) currDirName
        d <- readDirs pathList $ Directory (newDirInfo (takeDirectory currDirName) (takeFileName currDirName)) [] []
        readFiles pathList d

      readFiles :: [FilePath] -> Directory -> IO Directory
      readFiles pathList oldDir =
        let filePaths = filterM doesFileExist pathList
            files = (traverse mkFile =<< filePaths)
            newDir = fmap (Directory (dirInfo oldDir) (subDirs oldDir)) files
        in newDir

      readDirs :: [FilePath] -> Directory -> IO Directory
      readDirs pathList oldDir =
        let dirPaths = filterM doesDirectoryExist pathList
            dirs = (traverse readDirectory =<< dirPaths)
            newDir = fmap (\dirsList -> Directory (dirInfo oldDir) dirsList (subFiles oldDir)) dirs
        in newDir

      skipDot :: IO [FilePath] -> IO [FilePath]
      skipDot = fmap $ filter (\s -> show s /= "\".\"" && show s /= "\"..\"")

      toAbsolute :: String -> IO [FilePath] -> IO [FilePath]
      toAbsolute prefix = fmap $ map (\fp -> prefix ++ "/" ++ fp)

-- | <path to dir> <state>
-- Add VCSState to State
-- TODO: read VCS
readVCS :: String -> Directory -> IO Directory
readVCS _ = return

mkFile :: FilePath -> IO File
mkFile absolutePath = do
  let (filePath, fileName) = split absolutePath
  fileSize <- getFileSize absolutePath
  filePermissions <- getPermissions absolutePath
  let info = Info { path = filePath, name = fileName, size = fileSize, perm = filePermissions}
  File info <$> fileContent <*> return [] <*> time
  where
    split :: FilePath -> (FilePath, FilePath)
    split p = (takeDirectory p, takeFileName p)

    fileContent :: IO String
    fileContent = readFile absolutePath
    
    time :: IO UTCTime
    time = getAccessTime absolutePath

writeDirectoryState :: Directory -> IO ()
writeDirectoryState d = {-writeVCS vcs >>-} writeDir d
  where
--    writeVCS :: VCSDirectory -> IO ()
--    writeVCS _ = return ()

    writeDir :: Directory -> IO ()
    writeDir (Directory (DirInfo ci _ _) dirs files) = do
      createDirectoryIfMissing False (fullName ci)
      writeDirs  dirs
      writeFiles files

    writeDirs :: [Directory] -> IO ()
    writeDirs [] = return ()
    writeDirs (x : xs) = do
      writeDir x
      writeDirs xs

    writeFiles :: [File] -> IO ()
    writeFiles [] = return ()
    writeFiles (x : xs) = do
      length (content x) `seq` return ()
      if isWritableFile x
      then writeFile (getFullFileName x) (content x)
      else return ()
      writeFiles xs

isWritableFile :: File -> Bool
isWritableFile = writable . perm . commonInfo

isReadableFile :: File -> Bool
isReadableFile = readable . perm . commonInfo

type Data = String
type Name = String
