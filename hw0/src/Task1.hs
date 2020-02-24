{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  , eitherAssoclr
  , eitherAssocrl
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left  a)      = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoclr :: Either a (Either b c) -> Either (Either a b) c
eitherAssoclr (Left a)   = Left (Left a)
eitherAssoclr (Right bc) = case bc of
                             (Left  b) -> Left (Right b)
                             (Right c) -> Right c

eitherAssocrl :: Either (Either a b) c -> Either a (Either b c)
eitherAssocrl (Right c) = Right (Right c)
eitherAssocrl (Left ab) = case ab of
                            (Left  a) -> Left a
                            (Right b) -> Right (Left b)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherAssoclr, eitherAssocrl)
