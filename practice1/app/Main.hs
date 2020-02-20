--module Lib where

module Main where
-------------------------
-- Practice 1
-------------------------


------------------------------------------
-- Implement sorting of number sequences
--
-- You should implement three different
-- sorting algorithms.
--
-- Provide implementation in `solve` function.
-- Use `words` function to split whole input to words
-- and `readInt` to convert a word to an integer.
--
-- Your program will be given a sequence
-- of integers (written to stdin).
-- First integer takes value from range [0..2] and
-- signifies sorting algorithm to use.
-- Rest of integers is a sequence that is to be sorted.
--
-- You can compile and run your program with
-- `cabal run practice1`.
--
-------------------------------

main :: IO ()
main = interact solve

readInt :: String -> Int
readInt = read

showInt :: Int -> String
showInt = show

solve :: String -> String
solve s = case map readInt $ words s of
            []            -> "Error"
            (mode : list) -> concat $ map ((flip (++) " ") . showInt) $ sortWithMode mode list

sortWithMode :: Int -> [Int] -> [Int]
sortWithMode mode = case mode of
                      0 -> mergeSort
                      1 -> insertSort
                      2 -> quickSort
                      _ -> (\x -> x)

-- MERGESORT
split :: [a] -> ([a], [a])
split [] = ([], [])
split (x1 : x2 : xs) = case split xs of
                         (a, b) -> (x1 : a, x2 : b)
split (x : xs)       = case split xs of
                         (a, b) -> (x : a, b)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (l, r) = (f l, f r)

merge :: Ord a => ([a], [a]) -> [a]
merge ([], r) = r
merge (l, []) = l
merge (l : ls, r : rs) = if l < r then l : merge (ls, r : rs) else r : merge (l : ls, rs)

mergeSort :: Ord a => [a] -> [a]
mergeSort a@(_ : _ : _) = merge (mapPair mergeSort (split a))
mergeSort a = a


-- INSERTSORT
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort a  = let mn = findMin a in case mn of
                                        Nothing -> []
                                        Just x  -> x : (insertSort $ removeMin a)

findMin :: Ord a => [a] -> Maybe a
findMin [] = Nothing
findMin (x : xs) = let rec = findMin xs in case rec of
                                             Nothing -> Just x
                                             Just a  -> if x < a then Just x else Just a

removeMin :: Ord a => [a] -> [a]
removeMin a = case findMin a of
                Nothing -> []
                Just x  -> removeFrom x a

removeFrom :: Ord a => a -> [a] -> [a]
removeFrom _ []       = []
removeFrom x (l : ls) = if l == x then ls else l : (removeFrom x ls)

-- QUICKSORT
quickSort :: Ord a => [a] -> [a]
quickSort []       = []
quickSort (x : xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]
