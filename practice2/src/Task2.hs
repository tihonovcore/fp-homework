module Task2 where

import Data.Char (isDigit)
import Data.Char (digitToInt)
import Data.Char (toLower)

newtype FName = FName String

instance Eq FName where
  (FName left) == (FName right) = (left == right)

instance Ord FName where
  compare (FName left) (FName right) =
    let l = (splitToNumberAndString left) in
    let r = (splitToNumberAndString right) in
    case (l, r) of
      (FromNum lx ls, FromNum rx rs) ->
        case compare lx rx of
          EQ    -> compare (map toLower ls) (map toLower rs)
          other -> other
      (_, _) -> compare left right

data FromNum = FromNum Int String | Other

splitToNumberAndString :: String -> FromNum
splitToNumberAndString []       = Other
splitToNumberAndString (x : xs) =
  if isDigit x then
    splitWithAccum 0 (x : xs)
  else
    Other

splitWithAccum :: Int -> String -> FromNum
splitWithAccum accum [] = FromNum accum []
splitWithAccum accum (x : xs) =
  if isDigit x then
    splitWithAccum (accum * 10 + digitToInt x) xs
  else
    FromNum accum (x : xs)
