{-# LANGUAGE ScopedTypeVariables #-}

module Task2 where

import Control.Monad (when)
import Data.Hashable (Hashable(..))
import Data.Maybe (isJust)
import GHC.Conc (TVar, atomically, newTVar, readTVar, STM, writeTVar)

type Element k v = Maybe (k, v)

newtype ConcurrentHashTable k v = CHT { table :: TVar [Element k v] }

-- | Internal support class, uses for understanding
-- next position in array by modulo of array size
data Position = Position { startPos :: Int, currPos :: Int, size :: Int }

startSize :: Int
startSize = 4

-- | Create new concurrent hash table
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  let tableContent = replicate startSize Nothing
  newTable <- newTVar tableContent
  return $ CHT newTable

-- | Get value by key and concurrent hash table.
-- If there is not value with specified key returns Nothing.
getCHT :: forall k v . (Hashable k, Eq k, Eq v) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key cht = atomically $ do
  elems <- readTVar $ table cht
  let len = length elems
  let pos = hash key `mod` len
  findElement (Position pos pos len) elems
  where
    findElement :: Position -> [Element k v] -> STM (Maybe v)
    findElement position list =
      case (!!) list (currPos position) of
        Nothing      -> return Nothing
        Just element -> if fst element == key
                        then return $ Just (snd element)
                        else case nextIndex position of
                               Nothing   -> return Nothing
                               Just next -> findElement (position { currPos = next }) list

    nextIndex :: Position -> Maybe Int
    nextIndex p =
      let next = if currPos p + 1 /= size p
                 then currPos p + 1
                 else startPos p
      in if next == startPos p then Nothing else Just next

-- | Put value by key into concurrent hash table.
-- Value rewrites, if table already has value with
-- specified key.
-- Table spend O(n) space, where n - number of elements.
putCHT :: forall k v . (Hashable k, Eq k, Eq v) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value cht = atomically $ do
  elems <- readTVar $ table cht
  let len = length elems
  let pos = hash key `mod` len
  putElement key value (Position pos pos len) elems

  updatedElems <- readTVar $ table cht
  let realObjectCount = length $ filter isJust updatedElems
  when (realObjectCount * 4 `div` 3 >= len) $ rehash updatedElems (len * 2)
  where
    putElement :: k -> v -> Position -> [Element k v] -> STM ()
    putElement currKey currValue position list =
      case (!!) list (currPos position) of
        Nothing -> writeTVar (table cht) (set currKey currValue position list)
        Just  p -> if fst p == currKey
                   then writeTVar (table cht) (set currKey currValue position list)
                   else case nextPos position of
                          Nothing -> return ()
                          Just np -> putElement currKey currValue np list
    
    nextPos :: Position -> Maybe Position
    nextPos p = 
      let next = if currPos p + 1 == size p
                 then 0
                 else currPos p + 1
      in if next == startPos p then Nothing else Just (p { currPos = next })
    
    set :: k -> v -> Position -> [Element k v] -> [Element k v]
    set currKey currValue position list = setImpl list (currPos position)
      where
        setImpl :: [Element k v] -> Int -> [Element k v]
        setImpl []       _             = error "Internal error: hash great then array size"
        setImpl (x : xs) i | i == 0    = Just (currKey, currValue) : xs
                           | i >  0    = x : setImpl xs (i - 1)
                           | otherwise = error "Internal error: negative hash"

    rehash :: [Element k v] -> Int -> STM ()
    rehash oldTable newSize = do
      let newTable = replicate newSize Nothing
      writeTVar (table cht) newTable
      moveElements oldTable newSize

    moveElements :: [Element k v] -> Int -> STM ()
    moveElements [] _ = return ()
    moveElements (Nothing : xs) newSize = moveElements xs newSize
    moveElements ((Just (currKey, currValue)) : xs) newSize = do
      elems <- readTVar $ table cht
      let pos  = hash currKey `mod` newSize
      putElement currKey currValue (Position pos pos newSize) elems

      moveElements xs newSize

-- | Returns count elements in table.
sizeCHT :: forall k v . ConcurrentHashTable k v -> IO Int
sizeCHT cht = atomically $ do
  elems <- readTVar $ table cht
  return $ length $ filter isJust elems
