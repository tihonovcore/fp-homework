module Lib where

import DirectoryState

import Data.Time.Clock (UTCTime)
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import System.Directory (readable, writable)
import System.FilePath (splitPath, splitDirectories)
import Control.Monad (foldM)--import Debug.Trace (trace)
import Utils

-- |Nothing if dir not found
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

-- |Nothing if dir exists
mkdir :: Directory -> String -> OpMonad Directory
mkdir curr@(Directory currInfo dirs files) newDirName
  | dirAlreadyExistsIn dirs = throwError $ DirAlreadyExists newDirName
  | isWritable curr = return $ Directory currInfo newDirs files
  | otherwise = throwError NoPermissions
  where
    dirAlreadyExistsIn :: [Directory] -> Bool
    dirAlreadyExistsIn = foldr (\d -> (||) (getDirName d == newDirName)) False

    newDirs :: [Directory]
    newDirs = 
      let newDirPath = case currInfo of (DirInfo ci _ _) -> path ci in
      Directory (newDirInfo newDirPath newDirName) [] [] : dirs
    
    isWritable :: Directory -> Bool
    isWritable (Directory (DirInfo ci _ _) _ _) = writable (perm ci)

-- |Nothing if file exists
touch :: Directory -> String -> UTCTime -> OpMonad Directory
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

dir :: Directory -> String
dir = show

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

-- |Nothing if file not found
cat :: Directory -> Name -> OpMonad Data
cat (Directory _ _ files) expectedName = find files
  where
    find :: [File] -> OpMonad Data
    find [] = throwError $ FileNotFound expectedName
    find (x : xs) =
      if getFileName x == expectedName
      then if isReadableFile x
           then return $ content x
           else throwError NoPermissions
      else find xs

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

-- TODO: dir info
showInfo :: Directory -> Name -> OpMonad Data
showInfo (Directory _ _ files) expectedName = findInfo files
  where
    findInfo :: [File] -> OpMonad Data
    findInfo [] = throwError $ FileNotFound expectedName
    findInfo (x@(File common _ _ access) : xs) =
      if getFileName x == expectedName
      then return $ show common ++ "\nLast access: " ++ show access
      else findInfo xs

-- TODO: change file size
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

-- TODO: change file size
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

findFile :: Directory -> Name -> OpMonad Data
findFile (Directory _ dirs files) expectedName = searchInFiles files `opOr` searchInDirs dirs
  where
    searchInFiles :: [File] -> OpMonad Data
    searchInFiles [] = throwError $ ObjectNotFound expectedName
    searchInFiles (file : other) =
      if getFileName file == expectedName
      then return $ getFullFileName file
      else searchInFiles other

    searchInDirs :: [Directory] -> OpMonad Data
    searchInDirs [] = throwError $ ObjectNotFound expectedName
    searchInDirs (x : xs) = findFile x expectedName `opOr` searchInDirs xs
    
    opOr :: OpMonad Data -> OpMonad Data -> OpMonad Data
    opOr (Left _) r = r
    opOr success  _ = success 

multiCd :: FilePath -> Directory -> OpMonad Directory
multiCd longPath directory = foldM cd directory $ filter (\n -> n /= ".") $ splitDirectories longPath
