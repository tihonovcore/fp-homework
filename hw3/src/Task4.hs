{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Task4 where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Task3
import Data.Data (Typeable, (:~:) (..), eqT)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

translateToJS :: Expression a -> String
translateToJS = translate 0
  where
    -- TODO: needs to increment var index
    translate :: Int -> Expression a -> String
    translate _ (Int32 n) = show n
    translate _ (Boolean   n) = show n
    translate i (Plus l r) = translate i l <> " + "  <> translate i r
    translate i (Gt   l r) = translate i l <> " > "  <> translate i r
    translate i (And  l r) = translate i l <> " && " <> translate i r
    translate _ (RRVariable name _) = name
    translate i (Var  f) =
      let dv = defValue4 i f in
        case dv of
          (RRVariable name value) -> "var " <> name <> " = " <> translate i value <> "\n" <> translate (i + 1) (f dv)
          _                       -> error "impossible"
    translate i (While  c b) = "while (" <> translate i c <> ") {\n" <> translate i b <> "\n}\n"
    translate i (Apply (RRVariable name _) v) = name <> " = " <> translate i v
    translate i (Apply name value) = translate i name <> " = " <> translate i value --TODO: now `x * y = 3` is ok!
    translate i (Print e) = "console.log(" <> translate i e <> ")"
    translate _ End = ""
    translate i  (Seq l r) = translate i l <> "\n" <> translate i r
    

defValue4 :: Typeable t => Int -> (Expression t -> Expression k) -> Expression t
defValue4 index (_ :: Expression t -> Expression k) =
  let res = fromMaybe undefined $ defInt <|> defBool
  in  RRVariable ("var" ++ show index) res
  where
    defInt :: Maybe (Expression t)
    defInt = do
      Refl <- eqT @t @Int
      pure $ Int32 0

    defBool :: Maybe (Expression t)
    defBool = do
      Refl <- eqT @t @Bool
      pure $ Boolean False
