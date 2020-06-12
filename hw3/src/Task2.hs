{-# LANGUAGE ScopedTypeVariables #-}

module Task2 where

import GHC.Conc (TVar, atomically, newTVar, readTVar, STM, writeTVar)
import Data.Hashable (Hashable(..))

type Element k v = Maybe (k, v)

data ConcurrentHashTable k v = CHT { table :: TVar [Element k v] }

startSize :: Int
startSize = 4

--TODO: docs
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  let tableContent = replicate startSize Nothing
  newTable <- newTVar tableContent
  return $ CHT newTable

--TODO: docs
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
        Nothing      -> return Nothing -- element not found
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

--TODO: docs
putCHT :: forall k v . (Hashable k, Eq k, Eq v) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value cht = atomically $ do
  elems <- readTVar $ table cht
  let len = length elems
  let pos = hash key `mod` len
  putElement key value (Position pos pos len) elems
  
  updatedElems <- readTVar $ table cht
  let realObjectCount = foldr (\o a -> case o of Nothing  -> a
                                                 (Just _) -> a + 1) 0 updatedElems
  
  if realObjectCount * 4 `div` 3 >= len
  then rehash updatedElems (len * 2)
  else return ()
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
    
    nextPos :: Position -> Maybe Position --TODO: common?
    nextPos p = 
      let next = if currPos p + 1 == size p
                 then 0
                 else currPos p + 1
      in if next == startPos p then Nothing else Just (p { currPos = next })
    
    set :: k -> v -> Position -> [Element k v] -> [Element k v]
    set currKey currValue position list = setImpl list (currPos position)
      where
        setImpl :: [Element k v] -> Int -> [Element k v]
        setImpl []       _             = error "alalal" --TODO: readable message
        setImpl (x : xs) i | i == 0    = Just (currKey, currValue) : xs
                           | i >  0    = x : setImpl xs (i - 1)
                           | otherwise = error "lalala" --TODO: readable message
    
    rehash :: [Element k v] -> Int -> STM ()
    rehash oldTable newSize = do 
      let newTable = replicate newSize Nothing
      writeTVar (table cht) newTable
      moveElements oldTable newSize
    
    moveElements :: [Element k v] -> Int -> STM ()
    moveElements [] _ = return ()
    moveElements (Nothing : xs) sizeee = moveElements xs sizeee  --TODO: rename `sizeee`
    moveElements ((Just (currKey, currValue)) : xs) sizeee = do
      elems <- readTVar $ table cht
      let pos  = hash currKey `mod` sizeee
      putElement currKey currValue (Position pos pos sizeee) elems -- TODO: rename(?) `elem` -> `elements` everywhere
      
      moveElements xs sizeee

--TODO: docs
sizeCHT :: forall k v . ConcurrentHashTable k v -> IO Int
sizeCHT cht = atomically $ do
  elems <- readTVar $ table cht
  return $ foldr isJust 0 elems
  where
    isJust :: Element k v -> Int -> Int
    isJust Nothing x = x
    isJust (Just _) x = x + 1

--TODO: remove
foo :: IO (ConcurrentHashTable String String)
foo = newCHT

--TODO: docs
data Position = Position { startPos :: Int, currPos :: Int, size :: Int }
