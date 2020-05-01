module Lib
  ( Directory (..)
  , File (..)
  , Lib.findFile
  , cd
  , dir
  , mkdir
  , mkFile
  , touch
  ) where

import Control.Monad.State.Lazy
import DirectoryState
--import Debug.Trace (trace)

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
cd (Directory info dirs files) name = do
  (v, other) <- rmDir name dirs
  let u = Directory info other files
  let uv = if name == ".."
           then (u, invertName v)
           else (invertName u, v)
  Just $ uncurry addDir uv
  where
    rmDir :: String -> [Directory] -> Maybe (Directory, [Directory])
    rmDir = rmDirI []

    rmDirI :: [Directory] -> String -> [Directory] -> Maybe (Directory, [Directory])
    rmDirI _ _ [] = Nothing
    rmDirI visited expectedName (x : xs) =
      if expectedName == getName x
      then Just (x, visited ++ xs)
      else rmDirI (x : visited) expectedName xs

    invertName :: Directory -> Directory
    invertName (Directory (Info n invName s c r) d f) =
      Directory (Info invName n s c r) d f

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
      dirAlreadyExistsIn = foldr (\d -> (||) (getName d == newDirName)) False

      newDirs :: [Directory]
      newDirs = Directory (newInfo newDirName) [] [] : dirs

-- |Nothing if file exists
touch :: Directory -> String -> Maybe Directory
touch (Directory info dirs files) newFileName =
  if fileAlreadyExistsIn files
  then Nothing -- error $ "File not found: " ++ newFileName
  else Just $ Directory info dirs newFiles
    where
      fileAlreadyExistsIn :: [File] -> Bool
      fileAlreadyExistsIn [] = False
      fileAlreadyExistsIn (File name _ : xs) = name == newFileName || fileAlreadyExistsIn xs

      newFiles :: [File]
      newFiles = File newFileName "" : files

dir :: Directory -> String
dir = show

-- |Nothing if file not found
cat :: Directory -> File -> Maybe String
cat currentDir currentFile = undefined

-- TODO: rmFile
rm :: Directory -> String -> Directory
rm currentDir rmName = undefined

-- TODO: support recursive search in subDirs
findFile :: String -> StateT Directory Maybe File
findFile fileName = StateT $ \dir@(Directory _ _ files) -> traverse' (getFile files, dir) 
  where
    traverse' :: (Maybe a, b) -> Maybe (a, b)
    traverse' (Just l, r) = Just (l, r)
    traverse' _           = Nothing
    
    getFile :: [File] -> Maybe File
    getFile [] = Nothing
    getFile (file@(File currName _) : xs) =
      if currName == fileName
      then Just file
      else getFile xs
