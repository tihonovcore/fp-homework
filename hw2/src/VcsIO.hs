module VcsIO where

import DirectoryState
import System.Directory (doesDirectoryExist, createDirectory, removeDirectoryRecursive)
import System.FilePath (joinPath, takeDirectory, takeFileName, makeRelative)
import Data.List (sortBy)
import Data.Either (lefts, rights)
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Time.Calendar (Day(..))

-- | Read information about VCS from file system.
-- Skip zero commit and unite revisions to file.
readVcsDirectory :: FilePath -> IO Directory
readVcsDirectory vcsDirPath = upRevisions <$> readDirectoryState vcsDirPath
  where
    upRevisions :: Directory -> Directory
    upRevisions dir =
      let fileDirList = map action (subDirs dir) in
        Directory (dirInfo dir) (lefts fileDirList) (rights fileDirList)

    action :: Directory -> Either Directory File
    action dir =
      if isFile dir
      then Right $ mkFileWithRevisions (getFullDirName dir) (map content (skipZero $ sortByIndex $ subFiles dir))
      else Left $ upRevisions dir
      where
        skipZero :: [File] -> [File]
        skipZero = filter (\f -> getFileName f /= "0" || (length (content f) `seq` False))

        isFile :: Directory -> Bool
        isFile directory = null (subDirs directory) && (not . null $ subFiles directory)

        sortByIndex :: [File] -> [File]
        sortByIndex = sortBy (\l r -> compare (getFileName l) (getFileName r))

    mkFileWithRevisions :: FilePath -> [Data] -> File
    mkFileWithRevisions filePath revs =
      let ci = emptyInfo (takeDirectory filePath) (takeFileName filePath) in
      File ci "" revs (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))

-- | Append revisions form vcs-directory to files,
-- exists in work-directory. 
mergeDirAndVcs :: Directory -> Directory -> Directory
mergeDirAndVcs dir vcs = Directory (dirInfo dir) (handleDirs $ subDirs dir) (handleFiles $ subFiles dir)
  where
    handleDirs :: [Directory] -> [Directory]
    handleDirs [] = []
    handleDirs (d : other) =
      case getDirFromVcs (getDirName d) of
        Nothing -> d : handleDirs other
        Just x  -> mergeDirAndVcs d x : handleDirs other

    getDirFromVcs :: Name -> Maybe Directory
    getDirFromVcs expectedName = getDirByName (subDirs vcs)
      where
        getDirByName :: [Directory] -> Maybe Directory
        getDirByName [] = Nothing
        getDirByName (d : other) =
          if getDirName d == expectedName
          then Just d
          else getDirByName other

    handleFiles :: [File] -> [File]
    handleFiles [] = []
    handleFiles (f : other) =
      case getFileFromVcs (getFileName f) of
        Nothing -> f : handleFiles other
        Just x  -> mergeFiles f x : handleFiles other

    getFileFromVcs :: Name -> Maybe File
    getFileFromVcs expectedName = getFileByName (subFiles vcs)
      where
        getFileByName :: [File] -> Maybe File
        getFileByName [] = Nothing
        getFileByName (d : other) =
          if getFileName d == expectedName
          then Just d
          else getFileByName other

    mergeFiles :: File -> File -> File
    mergeFiles file vcsFile = File (commonInfo file) (content file) (revisions vcsFile) (lastAccess file)

-- | Save information about VCS into file system
-- For directory from ".../parent/current", VCS directory
-- will be in ".../parent/vcs/current".
--
-- For each file creates its own directory with file's name
-- as name. Directory will contains file "0" - zero commit.
-- Its uses for understanding that directory is container 
-- for revisions. Here also saves all revisions with names "1", "2", etc.
writeVcsState :: Directory -> IO ()
writeVcsState currDir = makeVcsDir >> writeDir currDir
  where
    makeVcsDir :: IO ()
    makeVcsDir = do
      let ci = dirCommonInfo currDir
      let pathToVcs = joinPath [path ci, "vcs"]
      removeDirectoryRecursive pathToVcs
      createDirectory pathToVcs

    toVcsPath :: FilePath -> FilePath
    toVcsPath filePath =
      let pathToVcsDir = (path . dirCommonInfo) currDir in
      let relativePath = makeRelative pathToVcsDir filePath in
        joinPath [pathToVcsDir, "vcs", relativePath]

    mkDirIfAbsent :: FilePath -> IO ()
    mkDirIfAbsent filePath =
      doesDirectoryExist filePath >>= (\a -> if a then return () else createDirectory filePath)

    writeDir :: Directory -> IO ()
    writeDir dir = do
      let pathToCurrDir = toVcsPath (getFullDirName dir)
      mkDirIfAbsent pathToCurrDir
      writeDirs  (subDirs  dir)
      writeFiles (subFiles dir)

    writeDirs :: [Directory] -> IO ()
    writeDirs = foldr ((>>) . writeDir) (return ())

    writeFiles :: [File] -> IO ()
    writeFiles = foldr ((>>) . writeVcsFile) (return ())

    writeVcsFile :: File -> IO ()
    writeVcsFile file = do
      let pathToCurrFile = toVcsPath (getFullFileName file)
      let zeroCommit = ""
      mkDirIfAbsent pathToCurrFile
      writeRevisions pathToCurrFile (zeroCommit : revisions file) 0

    writeRevisions :: FilePath -> [Data] -> Int -> IO ()
    writeRevisions _ [] _ = return ()
    writeRevisions filePath (rev : other) n = do
      let revPath = joinPath [filePath, show n]
      length rev `seq` return ()
      writeFile revPath rev
      writeRevisions filePath other (n + 1)
