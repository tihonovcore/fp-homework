module Task6
  ( false
  , foo
  , haroldDistr
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

-- `null` equals (\l -> case l of [] -> True
--                                _  -> False)
-- Let arg = mapMaybe foo "pole chudes ochen' chudesno"
-- `null $ arg` is redex
-- case l of arg -> True
--           _   -> False
-- Here `arg` should be in WHNF, so `arg` reduses to non-empty list and result of case is False
false :: Bool
false = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True -> Just $ exp pi
    False -> Nothing

haroldDistr :: (Either String a, Either String b)
haroldDistr = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
-- distributivity :: Either String AB -> Either String A, Either Stirng B
--(Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))
