{-# LANGUAGE Rank2Types #-}

module Task7 where

import Task5
import Lens.Micro ((%~), (&), (^..))
import System.FilePath (dropExtension, (<.>))

-- | Change non recursively extension of files in current 
-- directory to new one
changeExtension :: String -> FS -> FS
changeExtension new fs = fs & files.name %~ (\n -> dropExtension n <.> new)

-- | Returns list of names of subdirectories and files recursively
getAllNamesRecursive :: FS -> [FilePath]
getAllNamesRecursive fs = (fs ^.. files.name) ++ (fs ^.. dirs . name) ++ concatMap getAllNamesRecursive (fs ^.. dirs)

-- | Remove specified directory if its exists and empty
removeIfEmpty :: FilePath -> FS -> FS
removeIfEmpty n fs = fs & contents %~ filter passCheck
  where
    passCheck :: FS -> Bool
    passCheck = not . isExpected

    isExpected :: FS -> Bool
    isExpected (Dir cn []) = cn == n
    isExpected _           = False
