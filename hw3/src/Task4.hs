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

translateToJs :: Expression a -> IO String
translateToJs expr = do
  index <- newIORef 0
  translate index expr
  where
    translate :: IORef Int -> Expression a -> IO String
    translate _ (Int32   v) = return $ show v
    translate _ (Boolean v) = return $ show v
    translate _ (Str     v) = return $ show v
    translate _ (Dbl     v) = return $ show v

--    -- TODO: return in body?? (or by default returns last line?)
--    -- TODO: needs smth like `Variable` for printing functionName on call-site owtherwise it will inline
----    translate i (Fun  f scope) =
----      let dv = defValue4 i f in
----        case dv of
----          (RRVariable name _) -> "function func" <> show (i + 1) <> " (" <> name <> ") {\n" <>
----                                 translate (i + 1) (f dv) <>
----                                 "\n}\n" <> translate (i + 1) (scope f)
----          _                   -> error "impossible"
----      "function func" <> show i <> " (var" <> show (i + 1) <> ") {\n"
--
----    translate i (Fun2 body scope) =

    translate i (Plus l r) = translateBinOp i l r " + "
    translate i (Subs l r) = translateBinOp i l r " - "
    translate i (Mult l r) = translateBinOp i l r " * "
    translate i (Mod  l r) = translateBinOp i l r " % "
    translate i (Gt   l r) = translateBinOp i l r " > "
    translate i (And  l r) = translateBinOp i l r " && "
    translate i (Conc l r) = translateBinOp i l r " + "

    translate i (If c (Then t) (Else e)) = do
      cond <- translate i c
      thenBranch <- translate i t
      elseBranch <- translate i e
      return $ "if (" <> cond <> ") " <> thenBranch <> "\nelse " <> elseBranch
    translate i (While c b) = do
      cond <- translate i c
      body <- translate i b
      return $ "while (" <> cond <> ") {\n" <> body <> "\n}\n"

    translate _ (Variable name _) = return name
    translate i (Var  f) = do
      dv <- defaultValue i f
      case dv of
        (Variable name value) -> do
          val <- readIORef value >>= translate i
          scope <- translate i (f dv)
          return $ "var " <> name <> " = " <> val <> "\n" <> scope
        _ -> error "impsbl"

    translate i (Apply (Variable name _) v) = do
      right <- translate i v
      return $ name <> " = " <> right
    translate i (Apply name value) = do
      left  <- translate i name
      right <- translate i value
      return $ left <> " = " <> right --TODO: now `x * y = 3` is ok!!!
    translate i (Print e) = do
      arg <- translate i e
      return $ "console.log(" <> arg <> ")"

    translate i (Seq l r) = do
      left <- translate i l
      right <- translate i r
      return $ left <> "\n" <> right
    translate _ End = return ""

    translateBinOp :: IORef Int -> Expression a -> Expression a -> String -> IO String
    translateBinOp i l r op = do
      left  <- translate i l
      right <- translate i r
      return $ left <> op <> right
