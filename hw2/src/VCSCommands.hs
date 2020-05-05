module VCSCommands where

import DirectoryState
import Lib
import Control.Monad.Except (throwError)
import Data.Algorithm.Diff

-- TODO: add dir
add :: Name -> Directory -> OpMonad Directory
add expectedName (Directory di dirs files) = do
  newFiles <- mapFileIfExists files
  return $ Directory di dirs newFiles
  where
    mapFileIfExists :: [File] -> OpMonad [File]
    mapFileIfExists [] = throwError $ FileNotFound expectedName
    mapFileIfExists (file : xs) =
      if getFileName file == expectedName
      then return $ addContent file : xs
      else (\list -> return $ file : list) =<< mapFileIfExists xs

    addContent :: File -> File
    addContent file =
      if null $ revisions file
      then File (commonInfo file) (content file) [content file] (lastAccess file)
      else file

log :: Name -> Directory -> OpMonad Data
log expectedName (Directory _ _ files) = logRender <$> getRevisions files
  where
    getRevisions :: [File] -> OpMonad [Data]
    getRevisions [] = throwError $ FileNotFound expectedName
    getRevisions (file : xs) =
      if getFileName file == expectedName
      then return $ revisions file
      else getRevisions xs

    logRender :: [Data] -> Data
    logRender list = foldl (\output (n, cont) -> output ++ "\n" ++ renderRev n cont) "" (zip [1..] $ reverse list)
      where
        renderRev :: Int -> Data -> Data
        renderRev n cont = "### " ++ show n ++ ": \n" ++ cont

-- TODO: !null revs
-- TODO: comment
commit :: Name -> Directory -> OpMonad Directory
commit expectedName (Directory di dirs files) = Directory di dirs <$> commitFile files
  where
    commitFile :: [File] -> OpMonad [File]
    commitFile [] = throwError $ FileNotFound expectedName
    commitFile (file : xs) =
      if getFileName file == expectedName
      then return $ commitedFile file : xs
      else (:) file <$> commitFile xs

    commitedFile :: File -> File
    commitedFile (File ci cont revs la) = File ci cont (cont : revs) la

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
-- TODO: First -> Second is wrong change
-- Right one is [First] -> [Second]
-- ex. mergs landasd londan Right is `londa[s]n`, but expected `londa[]n` 
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
    getRev [] _ = throwError $ WrongRevisionIndex "???"
    getRev ((currIndex, rev) : xs) n =
      if currIndex == n
      then return rev
      else getRev xs n
    
    mergeRevs :: Data -> Data -> OpMonad Data
    mergeRevs l r = return $ mergeInternal $ getDiff l r
      where
        mergeInternal :: [Diff Char] -> Data
        mergeInternal [] = ""
        mergeInternal (First f : Second s : other) = 
          case strategy of 
            MSLeft -> f : mergeInternal other
            MSRight -> s : mergeInternal other
            MSBoth -> "\n" ++ [f] ++ " >>> " ++ [s] ++ "\n" ++ mergeInternal other
            MSInteractive -> error "interactive mode not support"
        mergeInternal (First f : other) = f : mergeInternal other
        mergeInternal (Second s : other) = s : mergeInternal other
        mergeInternal (Both a _ : other) = a : mergeInternal other
-- TODO: init
-- TODO: history
