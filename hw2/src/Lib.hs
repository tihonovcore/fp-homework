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
import System.FilePath.Posix
import Debug.Trace
import Control.Exception (throw)

-- |File <name> <content>
-- TODO: file info: size etc
data File = File String String

mtrace :: (Monad m, Show a) => m a -> m a
mtrace m = m >>= (\con -> trace (show con) m)

mkFile :: FilePath -> IO File
mkFile path = File name <$> content --(content >>= \con -> trace con content)
  where
    name :: FilePath
    name = takeFileName path --TODO: only name

    content :: IO String
    content = readFile path

instance Show File where
  show (File name con) = name ++ " " ++ con

--newtype Name = Name String

--TODO add `parent`
data Directory = Directory String [Directory] [File]

instance Show Directory where
  show = render ""

-- TODO: check new line
render :: String -> Directory -> String
render indent (Directory name subDirs files) =
  let newIndent = indent ++ "| " in
  name ++ "\n" ++ showWithIndentDir newIndent subDirs ++ showWithIndentFile newIndent files
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

-- |Nothing if dir not found
cd :: Directory -> String -> Maybe Directory
cd (Directory _ dirs _) nextDir = getDir nextDir dirs
  where
    getDir :: String -> [Directory] -> Maybe Directory
    getDir _ [] = Nothing
    getDir expectedName (x@(Directory currName _ _) : xs) =
      if expectedName == currName
      then Just x
      else getDir expectedName xs

-- |Nothing if dir exists
mkdir :: Directory -> String -> Maybe Directory
mkdir (Directory currDir dirs files) newDirName =
  if dirAlreadyExistsIn dirs
  then Nothing
  else Just $ Directory currDir newDirs files
    where
      dirAlreadyExistsIn :: [Directory] -> Bool
      dirAlreadyExistsIn [] = False
      dirAlreadyExistsIn (Directory name _ _ : xs) = name == newDirName || dirAlreadyExistsIn xs

      newDirs :: [Directory]
      newDirs = Directory newDirName [] [] : dirs

-- |Nothing if file exists
touch :: Directory -> String -> Maybe Directory
touch (Directory currDir dirs files) newFileName =
  if fileAlreadyExistsIn files
  then Nothing -- error $ "File not found: " ++ newFileName
  else Just $ Directory currDir dirs newFiles
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
