{-# LANGUAGE Rank2Types #-}

module Task6 where

import Lens.Micro (Traversal', filtered, (^.), traversed)
import Task5

type Name = FilePath

-- | Moves to subdirectory
cd :: Name -> Traversal' FS FS
cd n = dirs.filtered byName
  where
    byName :: FS -> Bool
    byName fs = fs^.name == n

-- | Lens for subdirectories and files of current directory
ls :: Traversal' FS Name
ls = contents.traversed.name

-- | Returns file name if its exists in current directory
file :: Name -> Traversal' FS Name
file n = files.name.filtered (n ==)
