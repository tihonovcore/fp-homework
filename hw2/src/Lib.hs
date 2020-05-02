module Lib where

import DirectoryState

import Data.Time.Clock (UTCTime)
import Control.Applicative ((<|>))--import Debug.Trace (trace)

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
cd :: Directory -> String -> Maybe Directory
cd (Directory info dirs files) nextDirName = do
  (v, other) <- rmDir nextDirName dirs
  let u = Directory info other files
  let uv = if nextDirName == ".."
           then (u, invertName v)
           else (invertName u, v)
  Just $ uncurry addDir uv
  where
    rmDir :: String -> [Directory] -> Maybe (Directory, [Directory])
    rmDir = rmDirI []

    rmDirI :: [Directory] -> String -> [Directory] -> Maybe (Directory, [Directory])
    rmDirI _ _ [] = Nothing
    rmDirI visited expectedName (x : xs) =
      if expectedName == getDirName x
      then Just (x, visited ++ xs)
      else rmDirI (x : visited) expectedName xs

    invertName :: Directory -> Directory
    invertName (Directory (DirInfo (Info pathi namei sizei permi) invName c) d f) =
                Directory (DirInfo (Info pathi invName sizei permi) namei c) d f

    addDir :: Directory -> Directory -> Directory
    addDir new (Directory n drs f) = Directory n (new : drs) f

-- |Nothing if dir exists
mkdir :: Directory -> String -> Maybe Directory
mkdir (Directory currInfo dirs files) newDirName =
  if dirAlreadyExistsIn dirs
  then Nothing
  else Just $ Directory currInfo newDirs files
    where
      dirAlreadyExistsIn :: [Directory] -> Bool
      dirAlreadyExistsIn = foldr (\d -> (||) (getDirName d == newDirName)) False

      newDirs :: [Directory]
      newDirs = Directory (newDirInfo newDirName) [] [] : dirs

-- |Nothing if file exists
touch :: Directory -> String -> UTCTime -> Maybe Directory
touch (Directory info dirs files) newFileName time =
  if fileAlreadyExistsIn files
  then Nothing
  else Just $ Directory info dirs newFiles
    where
      fileAlreadyExistsIn :: [File] -> Bool
      fileAlreadyExistsIn = foldr (\f -> (||) (getFileName f == newFileName)) False

      newFiles :: [File]
      newFiles = File (emptyInfo newFileName) "" time : files

dir :: Directory -> String
dir = show

-- |Nothing if file not found
cat :: Directory -> Name -> Maybe Data
cat (Directory _ _ files) expectedName = find files
  where
    find :: [File] -> Maybe Data
    find [] = Nothing
    find (x@(File _ content _) : xs) =
      if getFileName x == expectedName
      then Just content
      else find xs

rm :: Directory -> Name -> Maybe Directory
rm (Directory i dirs files) expectedName =
  let newFiles = rmFile [] files
      newDirs  = rmDir [] dirs in
  case (newFiles, newDirs) of
    (Nothing, Nothing) -> Nothing
    (Just fs, _      ) -> Just $ Directory i dirs fs
    (_      , Just ds) -> Just $ Directory i ds files
  where
    rmDir :: [Directory] -> [Directory] -> Maybe [Directory]
    rmDir _ [] = Nothing
    rmDir pref (x : xs) =
      if getDirName x == expectedName
      then Just $ pref ++ xs
      else rmDir (x : pref) xs
    
    rmFile :: [File] -> [File] -> Maybe [File]
    rmFile _ [] = Nothing
    rmFile pref (x : xs) =
      if getFileName x == expectedName
      then Just $ pref ++ xs
      else rmFile (x : pref) xs

-- TODO: dir info
showInfo :: Directory -> Name -> Maybe Data
showInfo (Directory _ _ files) expectedName = findInfo files
  where
    findInfo :: [File] -> Maybe Data
    findInfo [] = Nothing
    findInfo (x@(File common _ access) : xs) =
      if getFileName x == expectedName
      then Just $ show common ++ "\nLast access: " ++ show access
      else findInfo xs

-- TODO: change file size
rewriteFile :: Directory -> Name -> Data -> Maybe Directory
rewriteFile (Directory i d files) expectedName newContent =
  Just . Directory i d =<< write files
  where
    write :: [File] -> Maybe [File]
    write [] = Nothing
    write (x@(File info _ time) : xs) =
      if getFileName x == expectedName
      then Just $ File info newContent time : xs
      else fmap ((:) x) (write xs)

-- TODO: change file size
addToFile :: Directory -> Name -> Data -> Maybe Directory
addToFile (Directory i d files) expectedName newContent =
    Just . Directory i d =<< add files
  where
    add :: [File] -> Maybe [File]
    add [] = Nothing
    add (x@(File info content time) : xs) =
      if getFileName x == expectedName
      then Just $ File info (content ++ newContent) time : xs
      else fmap ((:) x) (add xs)

--TODO: return Maybe Data - path to file
findFile :: Directory -> Name -> Maybe File
findFile (Directory _ dirs files) expectedName = searchInFiles files <|> searchInDirs dirs
  where
    searchInFiles :: [File] -> Maybe File
    searchInFiles [] = Nothing
    searchInFiles (x : xs) =
      if getFileName x == expectedName
      then Just x
      else searchInFiles xs

    searchInDirs :: [Directory] -> Maybe File
    searchInDirs [] = Nothing
    searchInDirs (x : xs) = findFile x expectedName <|> searchInDirs xs
