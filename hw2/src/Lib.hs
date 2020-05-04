module Lib where

import DirectoryState

import Data.Time.Clock (UTCTime)
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import System.Directory (readable, writable)--import Debug.Trace (trace)

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
cd :: Directory -> String -> OpMonad Directory
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
  | dirAlreadyExistsIn dirs = throwError $ DirNotFound newDirName
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
touch (Directory info dirs files) newFileName time =
  if fileAlreadyExistsIn files
  then throwError $ FileAlreadyExists newFileName
  else return $ Directory info dirs newFiles
    where
      fileAlreadyExistsIn :: [File] -> Bool
      fileAlreadyExistsIn = foldr (\f -> (||) (getFileName f == newFileName)) False

      newFiles :: [File]
      newFiles = 
        let newPath = case info of (DirInfo ci _ _) -> fullName ci in
        File (emptyInfo newPath newFileName) "" time : files

dir :: Directory -> String
dir = show

data OpError = FileNotFound   Name
             | DirNotFound    Name
             | ObjectNotFound Name 
             | NoPermissions
             | FileAlreadyExists Name
             | DirAlreadyExists  Name
             | Seq OpError OpError
  deriving (Eq, Show)

type OpMonad = Either OpError

-- |Nothing if file not found
cat :: Directory -> Name -> OpMonad Data
cat (Directory _ _ files) expectedName = find files
  where
    find :: [File] -> OpMonad Data
    find [] = throwError $ FileNotFound expectedName
    find (x@(File _ content _) : xs) =
      if getFileName x == expectedName
      then if isReadable x
           then return content
           else throwError NoPermissions
      else find xs
    
    isReadable :: File -> Bool
    isReadable (File ci _ _) = readable (perm ci)

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
      then return $ pref ++ xs -- TODO: check permissions
      else rmDir (x : pref) xs
    
    rmFile :: [File] -> [File] -> OpMonad [File]
    rmFile _ [] = throwError $ FileNotFound expectedName
    rmFile pref (x : xs) =
      if getFileName x == expectedName
      then if isWritable x
           then return $ pref ++ xs
           else throwError NoPermissions
      else rmFile (x : pref) xs
    
    -- TODO: move to utils
    isWritable :: File -> Bool
    isWritable (File ci _ _) = writable (perm ci)

-- TODO: dir info
showInfo :: Directory -> Name -> OpMonad Data
showInfo (Directory _ _ files) expectedName = findInfo files
  where
    findInfo :: [File] -> OpMonad Data
    findInfo [] = throwError $ FileNotFound expectedName
    findInfo (x@(File common _ access) : xs) =
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
    write (x@(File info _ time) : xs) =
      if getFileName x == expectedName
      then if isWritable x
           then return $ File info newContent time : xs
           else throwError NoPermissions
      else fmap ((:) x) (write xs)
    
    -- TODO: move to utils
    isWritable :: File -> Bool
    isWritable (File ci _ _) = writable (perm ci)

-- TODO: change file size
addToFile :: Directory -> Name -> Data -> OpMonad Directory
addToFile (Directory i d files) expectedName newContent =
    Right . Directory i d =<< add files
  where
    add :: [File] -> OpMonad [File]
    add [] = throwError $ FileNotFound expectedName
    add (x@(File info content time) : xs) =
      if getFileName x == expectedName
      then if isWritable x 
           then return $ File info (content ++ newContent) time : xs
           else throwError NoPermissions
      else fmap ((:) x) (add xs)
    
    -- TODO: move to utils
    isWritable :: File -> Bool
    isWritable (File ci _ _) = writable (perm ci)

findFile :: Directory -> Name -> OpMonad Data
findFile (Directory _ dirs files) expectedName = searchInFiles files `opOr` searchInDirs dirs
  where
    searchInFiles :: [File] -> OpMonad Data
    searchInFiles [] = throwError $ ObjectNotFound expectedName
    searchInFiles (x : xs) =
      if getFileName x == expectedName
      then return $ getFullFileName x
      else searchInFiles xs

    searchInDirs :: [Directory] -> OpMonad Data
    searchInDirs [] = throwError $ ObjectNotFound expectedName
    searchInDirs (x : xs) = findFile x expectedName `opOr` searchInDirs xs
    
    opOr :: OpMonad Data -> OpMonad Data -> OpMonad Data
    opOr (Left _) r = r
    opOr success  _ = success 
