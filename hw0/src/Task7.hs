module Task7
  ( dorianGrey
  , leftRight
  , mods
  ) where

import Data.Either (lefts, rights)

dorianGrey :: Bool
dorianGrey = applyNullHead mapUncurryIdList
  where
    conc   = (++) :: String -> String -> String
    dorian = "Dorian" :: String
    grey   = " Grey" :: String

    concDorian = (conc dorian) :: String -> String

    pair = (concDorian, grey) :: (String -> String, String)
    list = [pair] :: [(String -> String, String)]

    uncurryId :: (a -> b, a) -> b
    uncurryId = (uncurry :: (a -> b -> c) -> (a, b) -> c) (id :: (a -> b) -> a -> b)

    mapUncurryIdList :: [String]
    mapUncurryIdList = (map :: (a -> b) -> [a] -> [b]) uncurryId list

    nullHead :: [String] -> Bool
    nullHead = compNull head_
      where
        comp  = (.) :: (b -> c) -> (a -> b) -> a -> c
        null_ = null :: [a] -> Bool
        head_ = head :: [String] -> String

        compNull = comp null_ :: (a -> [b]) -> a -> Bool

    applyNullHead :: [String] -> Bool
    applyNullHead = (($) :: (a -> b) -> a -> b) nullHead


leftRight :: [(Integer, Integer)]
leftRight = lambda list
  where
    argLeft :: Integer
    argLeft = ((+) :: Integer -> Integer -> Integer) (1 :: Integer) (2 :: Integer)

    argRight :: Integer
    argRight = ((^) :: Integer -> Integer -> Integer) (2 :: Integer) (6 :: Integer)

    l :: Either Integer Integer
    l = (Left :: a -> Either a b) argLeft

    r :: Either Integer Integer
    r = (Right :: b -> Either a b) argRight

    list :: [Either Integer Integer]
    list = [l, r]

    lambda :: [Either Integer Integer] -> [(Integer, Integer)]
    lambda = \x -> ((zip_ ((lefts_ x) :: [Integer])) :: [b] -> [(Integer, b)]) ((rights_ x) :: [Integer])
      where
        lefts_  = lefts :: [Either a b] -> [a]
        rights_ = rights :: [Either a b] -> [b]
        zip_    = zip :: [a] -> [b] -> [(a, b)]

mods :: Integer -> Bool
mods = let impl = impl_ in
       let isMod2 = isMod2_ in
       let isMod4 = isMod4_ in
       \x -> let isMod4x = (isMod4 (x :: Integer)) :: Bool in
             let isMod2x = (isMod2 (x :: Integer)) :: Bool in
             ((impl (isMod4x)) :: Bool -> Bool) isMod2x
         where
           impl_ :: Bool -> Bool -> Bool
           impl_ = \x y -> let or_ = (||) :: Bool -> Bool -> Bool in
                           let notx = ((not :: Bool -> Bool) (x :: Bool)) :: Bool in
                           ((or_ notx) :: Bool -> Bool) (y :: Bool)


           mod_  = mod :: Integer -> Integer -> Integer
           eq    = (==) :: Integer -> Integer -> Bool

           isMod2_ :: Integer -> Bool
           isMod2_ = \x -> let xmod  = (mod_ (x :: Integer)) :: Integer -> Integer in
                           let xmod2 = (xmod (2 :: Integer)) :: Integer in
                           ((eq xmod2) :: Integer -> Bool) (0 :: Integer)

           isMod4_ :: Integer -> Bool
           isMod4_ = \x -> let xmod  = (mod_ (x :: Integer)) :: Integer -> Integer in
                           let xmod4 = (xmod (4 :: Integer)) :: Integer in
                           ((eq xmod4) :: Integer -> Bool) (0 :: Integer)
