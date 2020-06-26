{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Task4 where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import NeatInterpolation
import Data.Text
import Task3

translateToJs :: Expression a -> IO Text
translateToJs expr = do
  index <- newIORef 0
  translate index expr
  where
    packs :: Show a => a -> Text
    packs = pack . show

    translate :: IORef Int -> Expression a -> IO Text
    translate _ (Int32   v) = return $ packs v
    translate _ (Boolean v) = return $ packs v
    translate _ (Str     v) = return $ packs v
    translate _ (Dbl     v) = return $ packs v

    translate i (Fun f s) = do
      var@(Variable unpackedVarName _) <- defaultValue i f

      index <- readIORef i
      writeIORef i (index + 1)

      let unpackedFuncName = "func" <> show index
      let funcName = pack unpackedFuncName
      let varName = pack unpackedVarName

      body  <- translateReturn i (f var)

      scope <- translate i (s (Runnable unpackedFuncName f))
      return [text|
               function $funcName($varName) {
                 $body
               }
               $scope
             |]

    translate i (Fun2 f s) = do
      var1@(Variable var1unpackedName _) <- defaultValue i f
      var2@(Variable var2unpackedName _) <- defaultValue i (flip f)

      index <- readIORef i
      writeIORef i (index + 1)

      let unpackedFuncName = "func" <> show index
      let funcName = pack unpackedFuncName
      let var1Name = pack var1unpackedName
      let var2Name = pack var2unpackedName

      body  <- translateReturn i (f var1 var2)

      scope <- translate i (s (Runnable2 unpackedFuncName f))
      return [text|
               function $funcName($var1Name, $var2Name) {
                 $body
               }
               $scope
             |]

    translate i (Call  (Runnable unpackedName _) a     ) = do
      let name = pack unpackedName
      arg <- translate i a
      return [text|$name($arg)|]
    translate i (Call2 (Runnable2 unpackedName _) a1 a2) = do
      let name = pack unpackedName
      arg1 <- translate i a1
      arg2 <- translate i a2
      return [text|$name($arg1, $arg2)|]

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
      return [text|
               if ($cond) {
                 $thenBranch
               } else {
                 $elseBranch
               }
             |]
    translate i (While c b) = do
      cond <- translate i c
      body <- translate i b
      return [text|
               while ($cond) {
                 $body
               }
             |]

    translate _ (Variable name _) = return $ pack name
    translate i (Var  f) = do
      dv <- defaultValue i f
      case dv of
        (Variable unpackedName value) -> do
          let name = pack unpackedName
          val <- readIORef value >>= translate i
          scope <- translate i (f dv)
          return [text|
                   var $name = $val
                   $scope
                 |]
        _ -> error "impsbl"

    translate i (Apply (Variable unpackedName _) v) = do
      let name = pack unpackedName
      right <- translate i v
      return [text|$name = $right|]
    translate i (Apply name value) = do
      left  <- translate i name
      right <- translate i value
      return [text|$left = $right|]
    translate i (Print e) = do
      arg <- translate i e
      return [text|console.log($arg)|]

    translate i (Seq l r) = do
      left <- translate i l
      right <- translate i r
      return $ left <> pack "\n" <> right
    translate _ End = return ""

    translateBinOp :: IORef Int -> Expression a -> Expression a -> String -> IO Text
    translateBinOp i l r op = do
      left  <- translate i l
      right <- translate i r
      return $ left <> pack op <> right

    translateReturn :: IORef Int -> Expression a -> IO Text
    translateReturn i (Seq l r) = do
      left  <- translate i l
      right <- translateReturn i r
      return $ left <> pack "\n" <> right
    translateReturn i other     = (<>) (pack "return ") <$> translate i other
