module VCSCommands where

import DirectoryState
import Lib
import Control.Monad.Except (throwError)
import Data.Algorithm.Diff

-- | Add file to VCS, makes initial commit. If there
-- isn't file - returns FileNotFound, if there isn't 
-- any commits - FileNotInVcs, if file isn't readable - NoPermissions.
add :: Name -> Directory -> OpMonad Directory
add expectedName (Directory di dirs files) = 
  if expectedName == ".." 
  then throwError NoPermissions
  else do
    newFiles <- mapFileIfExists files
    return $ Directory di dirs newFiles
  where
    mapFileIfExists :: [File] -> OpMonad [File]
    mapFileIfExists [] = throwError $ FileNotFound expectedName
    mapFileIfExists (file : xs) =
      if getFileName file == expectedName
      then if isReadableFile file
           then return $ addContent file : xs
           else throwError NoPermissions
      else (:) file <$> mapFileIfExists xs

    addContent :: File -> File
    addContent file =
      if null $ revisions file
      then File (commonInfo file) (content file) [content file] (lastAccess file)
      else file

-- | Show list of revisions by file.
-- If there isn't file - returns FileNotFound.
-- If file exists, but hasn't any commit - FileNotInVcs.
log :: Name -> Directory -> OpMonad Data
log expectedName (Directory _ _ files) = logRender <$> getRevisions files
  where
    getRevisions :: [File] -> OpMonad [Data]
    getRevisions [] = throwError $ FileNotFound expectedName
    getRevisions (file : xs) =
      if getFileName file == expectedName
      then if fileInVcs file
           then return $ revisions file
           else throwError $ FileNotInVcs expectedName
      else getRevisions xs

    logRender :: [Data] -> Data
    logRender list = foldl (\output (n, cont) -> output ++ "\n" ++ renderRev n cont) "" (zip [1..] $ reverse list)
      where
        renderRev :: Int -> Data -> Data
        renderRev n cont = "### " ++ show n ++ ": \n" ++ cont

    fileInVcs :: File -> Bool
    fileInVcs = not . null . revisions

-- | Add new revision. If there isn't file - returns
-- FileNotFound, if there isn't any commits - FileNotInVcs,
-- if file isn't readable - NoPermissions.
commit :: Name -> Directory -> OpMonad Directory
commit expectedName (Directory di dirs files) = Directory di dirs <$> commitFile files
  where
    commitFile :: [File] -> OpMonad [File]
    commitFile [] = throwError $ FileNotFound expectedName
    commitFile (file : xs) =
      if getFileName file == expectedName
      then if null $ revisions file
           then throwError $ FileNotInVcs expectedName
           else if isReadableFile file
           then return $ commitedFile file : xs
           else throwError NoPermissions
      else (:) file <$> commitFile xs

    commitedFile :: File -> File
    commitedFile (File ci cont revs la) = File ci cont (cont : revs) la

-- | Show revision by index. If isn't file - returns
-- FileNotFound, if there isn't revision with
-- specified number - WrongRevisionIndex.
showRevision :: Name -> Int -> Directory -> OpMonad Data
showRevision expectedName index (Directory _ _ files) = getRevision files
  where
    getRevision :: [File] -> OpMonad Data
    getRevision [] = throwError $ FileNotFound expectedName
    getRevision (file : xs) =
      if getFileName file == expectedName
      then getRevisionN index (reverse $ revisions file)
      else getRevision xs

    getRevisionN :: Int -> [Data] -> OpMonad Data
    getRevisionN _ [] = throwError $ WrongRevisionIndex expectedName
    getRevisionN n (rev : xs) | n == 1    = return rev
                              | n >  1    = getRevisionN (n - 1) xs
                              | otherwise = throwError $ WrongRevisionIndex expectedName

-- | Remove specified revision. If there isn't
-- revision with specified number - WrongRevisionIndex,
-- if there isn't file - returns FileNotFound.
removeRevision :: Name -> Int -> Directory -> OpMonad Directory
removeRevision expectedName index (Directory di dirs files) = Directory di dirs <$> updateRevision files
  where
    updateRevision :: [File] -> OpMonad [File]
    updateRevision [] = throwError $ FileNotFound expectedName
    updateRevision (file : xs) =
      if getFileName file == expectedName
      then (: xs) . (\revs -> File (commonInfo file) (content file) revs (lastAccess file)) <$> removeRevisionN index [] (reverse $ revisions file)
      else (:) file <$> updateRevision xs

    removeRevisionN :: Int -> [Data] -> [Data] -> OpMonad [Data]
    removeRevisionN _ _ [] = throwError $ WrongRevisionIndex expectedName
    removeRevisionN n pref (rev : xs) | n == 1    = return $ pref ++ xs
                                      | n >  1    = removeRevisionN (n - 1) (pref ++ [rev]) xs
                                      | otherwise = throwError $ WrongRevisionIndex expectedName

-- | Remove specified file from VCS (remove all commits).
-- If there file - returns FileNotFound.
rmFileFromVcs :: Name -> Directory -> OpMonad Directory
rmFileFromVcs expectedName (Directory di dirs files) = Directory di dirs <$> removeFile files
  where
    removeFile :: [File] -> OpMonad [File]
    removeFile [] = throwError $ FileNotFound expectedName
    removeFile (file : xs) =
      if getFileName file == expectedName
      then return $ File (commonInfo file) (content file) [] (lastAccess file) : xs
      else (:) file <$> removeFile xs

data MergeStrategy = MSLeft | MSRight | MSBoth | MSInteractive

-- | Merge two specified commits, and make new one
-- with result of merge.
merge :: Name -> Int -> Int -> MergeStrategy -> Directory -> OpMonad Directory
merge expectedName first second strategy (Directory di dirs files) = Directory di dirs <$> fileHandler files
  where
    fileHandler :: [File] -> OpMonad [File]
    fileHandler [] = throwError $ FileNotFound expectedName
    fileHandler (file : xs) =
      if getFileName file == expectedName
      then let newRevs = revHandler (reverse $ revisions file)
               newFiles revs = File (commonInfo file) (content file) revs (lastAccess file) : xs in
             newFiles <$> newRevs
      else (:) file <$> fileHandler xs

    revHandler :: [Data] -> OpMonad [Data]
    revHandler revs = do
      firstRev  <- getRev (zip [1..] revs) first
      secondRev <- getRev (zip [1..] revs) second
      mergeResult <- mergeRevs firstRev secondRev
      return $ mergeResult : reverse revs

    getRev :: [(Int, Data)] -> Int -> OpMonad Data
    getRev [] _ = throwError $ WrongRevisionIndex expectedName
    getRev ((currIndex, rev) : xs) n =
      if currIndex == n
      then return rev
      else getRev xs n
    
    mergeRevs :: Data -> Data -> OpMonad Data
    mergeRevs l r = return $ mergeInternal $ joinDiff $ getDiff l r
      where
        mergeInternal :: [Diff String] -> Data
        mergeInternal [] = ""
        mergeInternal (First f : Second s : other) =
          case strategy of
            MSLeft  -> f ++ mergeInternal other
            MSRight -> s ++ mergeInternal other
            MSBoth  -> "\n" ++ f ++ " >>> " ++ s ++ "\n" ++ mergeInternal other
            MSInteractive -> error "interactive mode not support"
        mergeInternal (First f : other) = f ++ mergeInternal other
        mergeInternal (Second s : other) = s ++ mergeInternal other
        mergeInternal (Both a _ : other) = a ++ mergeInternal other

        joinDiff :: [Diff Char] -> [Diff String]
        joinDiff [] = []
        joinDiff list@(x : _) =
          case x of
            First _ -> let (newFirst, other) = span isFirst list in joinFirst newFirst : joinDiff other
            Second _ -> let (newFirst, other) = span isSecond list in joinSecond newFirst : joinDiff other
            Both _ _ -> let (newFirst, other) = span isBoth list in joinBoth newFirst : joinDiff other
          where
            joinFirst :: [Diff Char] -> Diff String
            joinFirst [] = First ""
            joinFirst (First letter : other) = case joinFirst other of 
                                            First s -> First $ letter : s
                                            _       -> error "internal error"
            joinFirst _ = error "internal error"
            
            joinSecond :: [Diff Char] -> Diff String
            joinSecond [] = Second ""
            joinSecond (Second letter : other) = case joinSecond other of 
                                            Second s -> Second $ letter : s
                                            _       -> error "internal error"
            joinSecond _ = error "internal error"
            
            joinBoth :: [Diff Char] -> Diff String
            joinBoth [] = Both "" ""
            joinBoth (Both letter _ : other) = case joinBoth other of
                                                 Both s _ -> Both (letter : s) ""
                                                 _       -> error "internal error"
            joinBoth _ = error "internal error"
            
            isFirst :: Diff a -> Bool
            isFirst (First _) = True
            isFirst _         = False

            isSecond :: Diff a -> Bool
            isSecond (Second _) = True
            isSecond _         = False

            isBoth :: Diff a -> Bool
            isBoth (Both _ _) = True
            isBoth _         = False
