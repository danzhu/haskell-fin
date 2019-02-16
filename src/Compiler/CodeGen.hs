{-# LANGUAGE TemplateHaskell #-}

module Compiler.CodeGen
  ( codeGen
  ) where

import           Compiler.Ast
import           Control.Monad.RWS (RWS, evalRWS, get, put, tell)
import           Data.FileEmbed
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set

lib :: String
lib = $(embedStringFile "res/lib.js")


-- generator
type Generator = RWS () String Int

class Gen a where
  gen :: a -> Generator ()

instance Gen a => Gen (Node a) where
  gen Node{ nKind = a } = gen a

instance Gen Var where
  gen (Var v) = tell $ '_' : v

instance Gen Lit where
  gen kind = case kind of
    LInt i -> tell $ show i
    LStr s -> tell $ show s

instance Gen ExprKind where
  gen kind = case kind of
    ELit lit -> gen lit
    EVar var -> gen var
    EApp f a -> do
      gen f
      paren $ gen a
    EAbs arms -> do
      case arms of
        [Arm Node{ nKind = PVar arg } expr] -> fun arg $ gen expr
        [Arm Node{ nKind = PIgn } expr] -> do
            arg <- temp
            fun arg $ gen expr
        _ -> error "pattern matching not implemented"
    ESeq a b -> paren $ do
      gen a
      tell ","
      gen b
    ELet var val expr -> do
      fun var $ gen expr
      paren $ gen val
    EDef var val expr -> scope $ do
      tell "const "
      gen var
      tell "="
      gen val
      tell ";return "
      gen expr


-- pattern matching
-- data Match = MLit Lit
--            | MUnk
--            deriving (Ord, Eq)

-- type PatMatch = RWS () String (Map.Map (Set.Set Match) Int)

-- match :: Set.Set Match -> PatMatch Int
-- match m = do
--   st <- get
--   case Map.lookup m st of
--     Just i -> pure i
--     Nothing -> do
--       let i = Map.size st
--       update $ Map.insert m i
--       pure i

-- -- matchPat :: Pat -> PatMatch ()
-- -- matchPat (Node _ kind) = case kind of
-- --   PLit lit ->

-- -- matchArm :: Arm -> PatMatch ()
-- -- matchArm (Arm pat expr) = do
-- --   i <- match pat

-- genArms :: Var -> [Arm] -> Generator ()
-- genArms param (arm : arms) = do
--   genArms param arms
-- genArms _ [] = tell "(()=>{throw 'pattern fail'})()"


-- util
temp :: Generator Var
temp = do
  i <- get
  put $ i + 1
  pure $ Var $ "_t" ++ show i

paren :: Generator a -> Generator a
paren g = tell "(" *> g <* tell ")"

fun :: Var -> Generator a -> Generator a
fun var g = paren $ gen var *> tell "=>" *> g

scope :: Generator a -> Generator a
scope g = paren (tell "()=>{" *> g <* tell "}") <* paren (pure ())

genAst :: Ast -> Generator ()
genAst (Ast expr) = do
  tell "#!/usr/bin/env node\n"
  tell lib
  tell "console.log("
  gen expr
  tell ")\n"


codeGen :: Ast -> String
codeGen ast = snd $ evalRWS (genAst ast) () 0
