{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task4 where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Task3

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

    translate i (Fun f s) = do
      index <- readIORef i
      writeIORef i (index + 1)
      let funcName = "func" <> show index
      
      var@(Variable varName _) <- defaultValue i f
      body <- translateReturn i (f var)
      
      scope <- translate i (s (Runnable funcName f))
      return $ "function " <> funcName <> "(" <> varName <> ") {\n" <> body <> "\n}\n" <> scope

    translate i (Fun2 f s) = do
      index <- readIORef i
      writeIORef i (index + 1)
      let funcName = "func" <> show index
      
      var1@(Variable var1Name _) <- defaultValue i f
      var2@(Variable var2Name _) <- defaultValue i (flip f)
      body <- translateReturn i (f var1 var2)
      
      scope <- translate i (s (Runnable2 funcName f))
      return $ "function " <> funcName <> "(" <> var1Name <> ", " <> var2Name <> ") {\n" <> body <> "\n}\n" <> scope

    translate i (Call  (Runnable name _) a     ) = do
      arg <- translate i a
      return $ name <> "(" <> arg <> ")"
    translate i (Call2 (Runnable2 name _) a1 a2) = do
      arg1 <- translate i a1
      arg2 <- translate i a2
      return $ name <> "(" <> arg1 <> ", " <> arg2 <> ")"

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

    translateReturn :: IORef Int -> Expression a -> IO String
    translateReturn i (Seq l r) = do
      left  <- translate i l
      right <- translateReturn i r
      return $ left <> "\n" <> right
    translateReturn i other     = (++) "return " <$> translate i other
