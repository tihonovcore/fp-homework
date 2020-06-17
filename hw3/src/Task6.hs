{-# LANGUAGE Rank2Types #-}

module Task6 where

import Lens.Micro (Traversal', filtered, (^.), traversed)
import Task5

type Name = FilePath

cd :: Name -> Traversal' FS FS
cd n = dirs.filtered byName
  where
    byName :: FS -> Bool
    byName fs = fs^.name == n

ls :: Traversal' FS Name
ls = contents.traversed.name

file :: Name -> Traversal' FS Name
file n = files.name.filtered (n ==)
