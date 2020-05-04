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
data DirInfo = DirInfo CommonInfo String Int

instance Show CommonInfo where
  show info = "File \"" ++ name info ++ "\" at " ++ path info ++
    "\nFile size: " ++ show (size info) ++ "\n" ++ show (perm info)

instance Show DirInfo where
  show (DirInfo  common _ countFiles) = show common ++ "\nCount files: " ++ show countFiles

-- | FileInfo CommonInfo <content> <lastAccess>
data File = File CommonInfo Data UTCTime

instance Show File where
  show (File common _ _) = name common 
--  show (File common _ lastAccess) = show common ++ "\nLast access: " ++ show lastAccess

data Directory = Directory DirInfo [Directory] [File]

instance Show Directory where
  show = render ""

fullName :: CommonInfo -> Name
fullName info = path info ++ "/" ++ name info

getFileName :: File -> Name
getFileName (File ci _ _) = name ci

getFullFileName :: File -> Name
getFullFileName (File ci _ _) = fullName ci

getFileContent :: File -> Data
getFileContent (File _ content _) = content

getDirName :: Directory -> Name
getDirName (Directory (DirInfo ci _ _) _ _) = name ci

getFullDirName :: Directory -> Name
getFullDirName (Directory (DirInfo ci _ _) _ _) = fullName ci

-- TODO: print DirInfo too
render :: String -> Directory -> String
render indent curr@(Directory _ subDirs files) =
  let newIndent = indent ++ "| " in
  getFullDirName curr ++ "\n" ++ showWithIndentDir newIndent subDirs ++ showWithIndentFile newIndent files
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

emptyInfo :: FilePath -> Name -> CommonInfo
emptyInfo newPath newName =
  let newPerm = setOwnerReadable True $ setOwnerWritable True emptyPermissions in
    Info { path = newPath, name = newName,  size = 0, perm = newPerm }

-- |Make simple Info
newDirInfo :: FilePath -> Name -> DirInfo
newDirInfo newPath newName = 
  DirInfo (emptyInfo newPath newName) ".." 0

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
        pathList <- (toAbsolute name . skipDot . getDirectoryContents) name
        d <- readDirs pathList $ Directory (newDirInfo (takeDirectory name) (takeFileName name)) [] []
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
mkFile absolutePath = do
  let (filePath, fileName) = split absolutePath
  fileSize <- getFileSize absolutePath
  filePermissions <- getPermissions absolutePath
  let info = Info { path = filePath, name = fileName, size = fileSize, perm = filePermissions}
  File info <$> content <*> time
  where
    split :: FilePath -> (FilePath, FilePath)
    split p = (takeDirectory p, takeFileName p)

    content :: IO String
    content = readFile absolutePath
    
    time :: IO UTCTime
    time = getAccessTime absolutePath

writeDirectoryState :: DirectoryState -> IO ()
writeDirectoryState (DS vcs d) = writeVCS vcs >> writeDir d
  where
    writeVCS :: VCSDirectory -> IO ()
    writeVCS _ = return ()

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
      length (getFileContent x) `seq` return ()
      if isWritable x
      then writeFile (getFullFileName x) (getFileContent x)
      else return ()
      writeFiles xs

    isWritable :: File -> Bool
    isWritable (File info _ _) = readable $ perm info 

type Data = String
type Name = String
