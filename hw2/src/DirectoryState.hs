module DirectoryState where

import Control.Monad (filterM)
import Data.Time
import System.Directory
import System.FilePath
import System.IO.Strict

-- | Type-alias for file's content 
type Data = String
-- | Type-alias for file's name (only name, not absolute path)
type Name = String

-- | Information about file or directory
data CommonInfo = Info
  { path :: String
  , name :: String
  , size :: Integer
  , perm :: Permissions
  }

instance Show CommonInfo where
  show info = "Object \"" ++ name info ++ "\" at " ++ path info ++
    "\nSize: " ++ show (size info) ++ "\n" ++ show (perm info)

-- | Contains information about directory: path to directory, name,
-- size, permissions in CommonInfo, second name (if directory is
-- subdirectory - "..", otherwise original name. Names swaps in
-- `cd` operation) and directory's size
data DirInfo = DirInfo CommonInfo Name Int

instance Show DirInfo where
  show (DirInfo  common _ countFiles) = show common ++ "\nCount files: " ++ show countFiles

-- | Representation for file
data File = File
  { commonInfo :: CommonInfo
  , content :: Data
  , revisions :: [Data]
  , lastAccess :: UTCTime
  }

instance Show File where
  show = name . commonInfo

-- | Reptesentation for directory
data Directory = Directory
  { dirInfo  :: DirInfo
  , subDirs  :: [Directory]
  , subFiles :: [File]
  }

instance Show Directory where
  show = render ""

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

-- | Returns absolute path object by CommonInfo
fullName :: CommonInfo -> FilePath
fullName info = joinPath [path info, name info]

-- | Returns name of file
getFileName :: File -> Name
getFileName = name . commonInfo

-- | Returns absolute path to file
getFullFileName :: File -> FilePath
getFullFileName = fullName . commonInfo

-- | Returns name of directory
getDirName :: Directory -> Name
getDirName = name . dirCommonInfo

-- | Returns absolute path of directory
getFullDirName :: Directory -> Name
getFullDirName = fullName . dirCommonInfo

-- | Returns directory's CommonInfo
dirCommonInfo :: Directory -> CommonInfo
dirCommonInfo (Directory (DirInfo ci _ _) _ _) = ci

-- | Make simple CommonInfo
emptyInfo :: FilePath -> Name -> CommonInfo
emptyInfo = infoWithPermissions $ emptyPermissions { readable = True, writable = True }

-- | Make simple CommonInfo with users permissions
infoWithPermissions :: Permissions -> FilePath -> Name -> CommonInfo
infoWithPermissions permissions newPath newName = Info { path = newPath, name = newName, size = 0, perm = permissions }

-- | Make simple DirInfo. Default permissions: readable, writable
newDirInfo :: FilePath -> Name -> DirInfo
newDirInfo newPath newName = DirInfo (emptyInfo newPath newName) ".." 0

-- | Make simple DirInfo with users permissions
newDirInfoWithPermissions :: Permissions -> FilePath -> Name -> DirInfo
newDirInfoWithPermissions permissions newPath newName = DirInfo (infoWithPermissions permissions newPath newName) ".." 0

-- |Read DirectoryState from file system
readDirectoryState :: FilePath -> IO Directory
readDirectoryState = readDirectory
  where
    readDirectory :: FilePath -> IO Directory
    readDirectory currDirName = do
      permissions <- getPermissions currDirName
      let emptyDir = Directory (newDirInfoWithPermissions permissions (takeDirectory currDirName) (takeFileName currDirName)) [] []
      if readable permissions
      then do
        pathList <- (toAbsolute currDirName . skipDot . getDirectoryContents) currDirName
        d <- readDirs pathList emptyDir
        readFiles pathList d
      else return emptyDir

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

    toAbsolute :: FilePath -> IO [FilePath] -> IO [FilePath]
    toAbsolute prefix = fmap $ map (\fp -> prefix ++ "/" ++ fp)

-- | Read file from file system (in case file of readable,
-- otherwise content is empty string). Read and save to
-- CommonInfo file size, permissions and time of last access.
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

    fileContent :: IO Data
    fileContent = do
      isReadable <- readable <$> getPermissions absolutePath
      if isReadable
      then System.IO.Strict.readFile absolutePath
      else return ""

    time :: IO UTCTime
    time = getAccessTime absolutePath

-- | Save representation of directory to file system
writeDirectoryState :: Directory -> IO ()
writeDirectoryState = writeDir
  where
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
      if isWritableFile x
      then writeFile (getFullFileName x) (content x)
      else return ()
      writeFiles xs

-- | Returns True iff user has writable permission for this file
isWritableFile :: File -> Bool
isWritableFile = writable . perm . commonInfo

-- | Returns True iff user has readable permission for this file
isReadableFile :: File -> Bool
isReadableFile = readable . perm . commonInfo
