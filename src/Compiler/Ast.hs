module Compiler.Ast
  ( Var(..)
  , Node(..)
  , Lit(..)
  , Pat
  , PatKind(..)
  , Arm(..)
  , Expr
  , ExprKind(..)
  , Ast(..)
  , mkNode
  , prePat
  , preAst
  , display
  ) where

import           Compiler.Data
import           Compiler.Type
import qualified Control.Monad.RWS as RWS
import           Data.Foldable (for_)
import           Data.Traversable (for)

newtype Var = Var String
            deriving (Eq, Ord)

data Node a = Node { nType :: Type
                   , nPos  :: SrcPos
                   , nKind :: a }
            deriving Show

data Lit = LInt Int
         | LStr String
         deriving (Eq, Ord)

type Pat = Node PatKind

data PatKind = PLit Lit
             | PVar Var
             | PIgn
             deriving Show

data Arm = Arm Pat Expr
         deriving Show

type Expr = Node ExprKind

data ExprKind = ELit Lit
              | EVar Var
              | EApp Expr Expr
              | EAbs [Arm]
              | ESeq Expr Expr
              | ELet Var Expr Expr
              | EDef Var Expr Expr
              deriving Show

newtype Ast = Ast Expr


mkNode :: SrcPos -> a -> Node a
mkNode = Node $ TVar $ TypeVar ""

prePat :: Monad m => (t -> m Pat) -> t -> m Pat
prePat f n = do
  n'@Node{ nKind = kind } <- f n
  kind' <- case kind of
    PLit{} -> pure kind
    PVar{} -> pure kind
    PIgn{} -> pure kind
  pure $ n'{ nKind = kind' }

preAst :: (Monad f) => (Expr -> f Expr) -> (Pat -> f Pat) -> Ast -> f Ast
preAst fExpr fPat (Ast expr) = Ast <$> preExpr expr
  where preArm (Arm p e) = Arm <$> prePat fPat p <*> preExpr e
        preExpr n = do
          n'@Node{ nKind = kind } <- fExpr n
          kind' <- case kind of
                     ELit{}     -> pure kind
                     EVar{}     -> pure kind
                     EApp f a   -> EApp <$> preExpr f <*> preExpr a
                     EAbs as    -> EAbs <$> for as preArm
                     ESeq a b   -> ESeq <$> preExpr a <*> preExpr b
                     ELet v a b -> ELet v <$> preExpr a <*> preExpr b
                     EDef v a b -> EDef v <$> preExpr a <*> preExpr b
          pure $ n'{ nKind = kind' }


-- indent, output, ()
type Displayer = RWS.RWS Int String ()

recur :: (Display d) => d -> Displayer ()
recur d = RWS.local (+ 1) $ do
  ind <- RWS.ask
  RWS.tell $ replicate (ind * 2) ' '
  disp d

title :: String -> Displayer ()
title s = RWS.tell $ s ++ "\n"

display :: (Display d) => d -> String
display d = snd $ RWS.evalRWS (disp d) 0 ()

class Display a where
  disp :: a -> Displayer ()

instance (Display a) => Display (Node a) where
  disp Node{ nType = tp, nKind = a } = do
    RWS.tell $ show tp ++ ": "
    disp a

instance Show Var where
  show (Var v) = show v

instance Show Lit where
  show (LInt i) = show i
  show (LStr s) = show s

instance Display PatKind where
  disp (PLit l) = title $ "lit " ++ show l
  disp (PVar v) = title $ "var " ++ show v
  disp (PIgn)   = title $ "ign"

instance Display Arm where
  disp (Arm p e) = do
    title "arm"
    recur p
    recur e

instance Display ExprKind where
  disp (ELit l) = title $ "lit " ++ show l
  disp (EVar v) = title $ "var " ++ show v
  disp (EApp f a) = do
    title "app"
    recur f
    recur a
  disp (EAbs as) = do
    title "abs"
    for_ as recur
  disp (ESeq a b) = do
    title "seq"
    recur a
    recur b
  disp (ELet var val expr) = do
    title $ "let " ++ show var
    recur val
    recur expr
  disp (EDef var val expr) = do
    title $ "def " ++ show var
    recur val
    recur expr

instance Display Ast where
  disp (Ast expr) = disp expr

instance Show Ast where
  show = display
