module Lib where

import DirectoryState

import Data.Time.Clock (UTCTime)
import Control.Monad.Except (throwError)
import System.Directory (readable, writable)
import System.FilePath (splitDirectories)
import Control.Monad (foldM)

-- | Go to subdirectory by name. Returns Right subdirectory,
-- or Left DirNotFound if directory in absent,
-- or Left NoPermissions if directory is not readable.
-- 
-- u> cd v
-- delete `v` from `u`'s directories
-- rename `u` to `..`
-- add `u` to `v`'s directories
--
-- v> cd ..
-- delete `..` from `v`'s directories
-- rename `..` to `u`
-- add `v` to `u`'s directories
cd :: Directory -> Name -> OpMonad Directory
cd (Directory info dirs files) expectedName = do
  (v, other) <- rmDir dirs
  let u = Directory info other files
  let uv = if expectedName == ".."
           then (u, invertName v)
           else (invertName u, v)
  return $ uncurry addDir uv
  where
    rmDir :: [Directory] -> OpMonad (Directory, [Directory])
    rmDir = rmDirI []

    rmDirI :: [Directory] -> [Directory] -> OpMonad (Directory, [Directory])
    rmDirI _ [] = throwError $ DirNotFound expectedName
    rmDirI visited (x : xs) =
      if expectedName == getDirName x
      then if isReadable x 
           then return (x, visited ++ xs)
           else throwError NoPermissions
      else rmDirI (x : visited) xs

    invertName :: Directory -> Directory
    invertName (Directory (DirInfo (Info pathi namei sizei permi) invName c) d f) =
                Directory (DirInfo (Info pathi invName sizei permi) namei c) d f

    addDir :: Directory -> Directory -> Directory
    addDir new (Directory n drs f) = Directory n (new : drs) f
    
    isReadable :: Directory -> Bool
    isReadable (Directory (DirInfo ci _ _) _ _) = readable (perm ci)

-- | Create new Directory with new directory
-- If current directory contains file or another
-- directory with expected name, it returns Left FileAlreadyExists
-- or Left DirAlreadyExists.
mkdir :: Directory -> Name -> OpMonad Directory
mkdir curr@(Directory currInfo dirs files) newDirName
  |  dirAlreadyExistsIn dirs  = throwError $  DirAlreadyExists newDirName
  | fileAlreadyExistsIn files = throwError $ FileAlreadyExists newDirName
  | isWritable curr = return $ Directory currInfo newDirs files
  | otherwise = throwError NoPermissions
  where
    dirAlreadyExistsIn :: [Directory] -> Bool
    dirAlreadyExistsIn = foldr (\d -> (||) (getDirName d == newDirName)) False

    fileAlreadyExistsIn :: [File] -> Bool
    fileAlreadyExistsIn = foldr (\f -> (||) (getFileName f == newDirName)) False

    newDirs :: [Directory]
    newDirs = 
      let newDirPath = getFullDirName curr in
      Directory (newDirInfo newDirPath newDirName) [] [] : dirs
    
    isWritable :: Directory -> Bool
    isWritable (Directory (DirInfo ci _ _) _ _) = writable (perm ci)

-- | Returns new Directory with new file,
-- if file already exists in directory - FileAlreadyExists,
-- if directory already exists in current directory - DirAlreadyExists,
-- or NoPermissions if current directory isn't writable.
touch :: Directory -> Name -> UTCTime -> OpMonad Directory
touch directory@(Directory info dirs files) newFileName time
  | isNotWritable directory   = throwError NoPermissions
  | fileAlreadyExistsIn files = throwError $ FileAlreadyExists newFileName
  | dirAlreadyExistsIn  dirs  = throwError $ DirAlreadyExists  newFileName
  | otherwise                 = return $ directory { subFiles = newFiles }
    where
      fileAlreadyExistsIn :: [File] -> Bool
      fileAlreadyExistsIn = foldr (\f -> (||) (getFileName f == newFileName)) False

      dirAlreadyExistsIn :: [Directory] -> Bool
      dirAlreadyExistsIn = foldr (\f -> (||) (getDirName f == newFileName)) False

      newFiles :: [File]
      newFiles = 
        let newPath = case info of (DirInfo ci _ _) -> fullName ci in
        File (emptyInfo newPath newFileName) "" [] time : files

      isNotWritable :: Directory -> Bool
      isNotWritable = not . writable . perm . dirCommonInfo

data Object = ODirectory Directory | OFile File

-- | Show subdirectories and files by directory.
showDirsAndFiles :: Directory -> String
showDirsAndFiles directory = showObjects $ map ODirectory (subDirs directory) ++ map OFile (subFiles directory)
  where
    showObjects :: [Object] -> String
    showObjects (x : y : other) = "├─ " ++ showObject x ++ "\n" ++ showObjects (y : other)
    showObjects [x] = "└─ " ++ showObject x
    showObjects _   = ""

    showObject :: Object -> String
    showObject (OFile f) = getFileName f
    showObject (ODirectory d) = getDirName d

data OpError = FileNotFound   Name
             | DirNotFound    Name
             | ObjectNotFound Name 
             | FileNotInVcs   Name
             | NoPermissions
             | FileAlreadyExists  Name
             | DirAlreadyExists   Name
             | WrongRevisionIndex Name
             | Seq OpError OpError
  deriving (Eq, Show)

type OpMonad = Either OpError

-- | Returns file content, NoPermissions if file is
-- not readable and FileNotFound if directory does
-- not contains file with specified name.
cat :: Directory -> Name -> OpMonad Data
cat dir expectedName = find $ subFiles dir
  where
    find :: [File] -> OpMonad Data
    find [] = throwError $ FileNotFound expectedName
    find (file : other) =
      if getFileName file == expectedName
      then if isReadableFile file
           then return $ content file
           else throwError NoPermissions
      else find other

-- | Create new Directory without specified object,
-- in case it of exists, or NoPermissions if directory
-- is not writable, FileNotFound if there isn't file with
-- specified name or DirNotFound if there isn't directory
-- with specified name.
rm :: Directory -> Name -> OpMonad Directory
rm (Directory i dirs files) expectedName =
  let newFiles = rmFile [] files
      newDirs  = rmDir [] dirs in
  case (newFiles, newDirs) of
    (Left e1 , Left  e2) -> throwError $ Seq e1 e2
    (Right fs, _       ) -> return $ Directory i dirs fs
    (_       , Right ds) -> return $ Directory i ds files
  where
    rmDir :: [Directory] -> [Directory] -> OpMonad [Directory]
    rmDir _ [] = throwError $ DirNotFound expectedName
    rmDir pref (x : xs) =
      if getDirName x == expectedName
      then if getDirName x == ".." || (not . writable . perm . dirCommonInfo) x
           then throwError NoPermissions
           else return $ pref ++ xs
      else rmDir (x : pref) xs
    
    rmFile :: [File] -> [File] -> OpMonad [File]
    rmFile _ [] = throwError $ FileNotFound expectedName
    rmFile pref (x : xs) =
      if getFileName x == expectedName
      then if isWritableFile x
           then return $ pref ++ xs
           else throwError NoPermissions
      else rmFile (x : pref) xs

-- | Show information about directory of file
showInfo :: Directory -> Name -> OpMonad Data
showInfo directory expectedName = fileFindInfo (subFiles directory) `opOr` findDirInfo (subDirs directory)
  where
    fileFindInfo :: [File] -> OpMonad Data
    fileFindInfo [] = throwError $ FileNotFound expectedName
    fileFindInfo (x@(File common _ _ access) : other) =
      if getFileName x == expectedName
      then return $ show common ++ "\nLast access: " ++ show access
      else fileFindInfo other

    findDirInfo :: [Directory] -> OpMonad Data
    findDirInfo [] = throwError $ DirNotFound expectedName
    findDirInfo (currDir : other) =
      if getDirName currDir == expectedName
      then return $ show (dirInfo currDir)
      else findDirInfo other

    opOr :: OpMonad Data -> OpMonad Data -> OpMonad Data
    opOr (Left   e1) (Left   e2) = throwError $ Seq e1 e2
    opOr (Right res) _           = Right res
    opOr _           (Right res) = Right res

-- | Change file content to new one.
-- Return FileNotFound if file doesn't exists
-- in current directory or NoPermissions if file
-- is not writable.
rewriteFile :: Directory -> Name -> Data -> OpMonad Directory
rewriteFile (Directory i d files) expectedName newContent =
  Right . Directory i d =<< write files
  where
    write :: [File] -> OpMonad [File]
    write [] = throwError $ FileNotFound expectedName
    write (file : other) =
      if getFileName file == expectedName
      then if isWritableFile file
           then return $ file { content = newContent } : other
           else throwError NoPermissions
      else fmap ((:) file) (write other)

-- | Addend content to exiting file.
-- Return FileNotFound if file doesn't exists
-- in current directory or NoPermissions if file
-- is not writable.
append :: Directory -> Name -> Data -> OpMonad Directory
append (Directory i d files) expectedName newContent =
    Right . Directory i d =<< add files
  where
    add :: [File] -> OpMonad [File]
    add [] = throwError $ FileNotFound expectedName
    add (file : other) =
      if getFileName file == expectedName
      then if isWritableFile file
           then return $ file { content = content file ++ newContent } : other
           else throwError NoPermissions
      else fmap ((:) file) (add other)

-- | Searched file in current directory and in subdirectories.
-- Returns FileNotFound if there isn't file with specified name,
-- or absolute path to file.
findFile :: Directory -> Name -> OpMonad Data
findFile (Directory _ dirs files) expectedName = searchInFiles files `opOr` searchInDirs dirs
  where
    searchInFiles :: [File] -> OpMonad Data
    searchInFiles [] = throwError $ FileNotFound expectedName
    searchInFiles (file : other) =
      if getFileName file == expectedName
      then return $ getFullFileName file
      else searchInFiles other

    searchInDirs :: [Directory] -> OpMonad Data
    searchInDirs [] = throwError $ FileNotFound expectedName
    searchInDirs (x : xs) = findFile x expectedName `opOr` searchInDirs xs
    
    opOr :: OpMonad Data -> OpMonad Data -> OpMonad Data
    opOr (Left _) r = r
    opOr success  _ = success 

-- | If path contains intermediate directories
-- `multiCd` goes through all of them
multiCd :: FilePath -> Directory -> OpMonad Directory
multiCd longPath directory = foldM cd directory $ filter (\n -> n /= ".") $ splitDirectories longPath
