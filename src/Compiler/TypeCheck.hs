{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Compiler.TypeCheck
  ( TypeError(..)
  , TypeErrorKind(..)
  , typeAst
  ) where

import           Compiler.Ast
import           Compiler.Data
import           Compiler.Type
import           Control.Arrow (first)
import           Control.Monad (foldM)
import           Control.Monad.Except (throwError)
import qualified Control.Monad.RWS as RWS
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Debug.Trace

data TypeError = TypeErr SrcPos TypeErrorKind
data TypeErrorKind = UnboundVar Var
                   | UnifyError Type Type
                   | OccursError TypeVar Type

instance Show TypeErrorKind where
  show (UnboundVar var) = "unbound variable " ++ show var
  show (UnifyError t1 t2) = "cannot unify " ++ show t1 ++ " with " ++ show t2
  show (OccursError var tp) = "type cycle detected: " ++ show var ++ " in " ++ show tp

litType :: Lit -> Type
litType lit = TNat $ case lit of
  LInt{} -> NInt
  LStr{} -> NStr

type Context = Map.Map Var Scheme

type Infer = RWS.RWST Context [Constraint] (Set.Set TypeVar, Int) (Either TypeError)

newtype Subst = Subst (Map.Map TypeVar Type)
              deriving (Show)

instance Semigroup Subst where
  Subst a <> Subst b = Subst $ b `Map.union` Map.map (subst $ Subst b) a

instance Monoid Subst where
  mempty = Subst Map.empty

class Substitutable a where
  subst :: Subst -> a -> a

instance Substitutable Type where
  subst subs@(Subst sm) tp = case tp of
    TNat{}   -> tp
    TVar var -> Map.findWithDefault tp var sm
    TFun p r -> TFun (subst subs p) (subst subs r)

instance Substitutable Constraint where
  subst subs (Constraint pos a b) = Constraint pos (subst subs a) (subst subs b)

instance Substitutable Ast where
  subst subs = runIdentity . preOrder sub sub
    where sub n@Node{ nType = tp } = pure n{ nType = subst subs tp }

instance (Substitutable a) => Substitutable [a] where
  subst = map . subst

freshVar :: Infer Type
freshVar = do
  (vs, i) <- RWS.get
  let var = TypeVar $ "_t" ++ show i
  RWS.put (Set.insert var vs, i + 1)
  pure $ TVar $ var

usedVars :: Infer (Set.Set TypeVar)
usedVars = RWS.gets fst

freeVars :: Type -> Set.Set TypeVar
freeVars tp = case tp of
  TNat{}   -> Set.empty
  TVar var -> Set.singleton var
  TFun p r -> freeVars p <> freeVars r

occurs :: TypeVar -> Type -> Bool
occurs tv a = tv `Set.member` freeVars a

instantiate :: Scheme -> Infer Type
instantiate (Forall cns tvs tp) = do
  let fold ss tv = (\var -> Map.insert tv var ss) <$> freshVar
  subs <- Subst <$> foldM fold Map.empty tvs
  RWS.tell $ subst subs cns
  pure $ subst subs tp

generalize :: [Constraint] -> Set.Set TypeVar -> Type -> Scheme
generalize cns tvs tp = do
  traceShow ("generalize", cns, tvs, tp) $ Forall cns tvs tp

patVars :: Pat -> [(Var, Type)]
patVars Node{ nType = tp, nKind = kind } = case kind of
  PLit{} -> []
  PVar v -> [(v, tp)]
  PIgn   -> []

class Typeable a where
  constraint :: a -> Infer a

instance Typeable Pat where
  constraint node@Node{ nKind = kind } = do
    tp <- case kind of
      PLit lit -> pure $ litType lit
      PVar{}   -> freshVar
      PIgn     -> freshVar
    pure $ node{ nType = tp }

instance Typeable Expr where
  constraint node@Node{ nPos = pos, nKind = kind } = do
    (tp, kind') <- case kind of
      ELit lit -> pure (litType lit, ELit lit)
      EVar var -> do
        scm <- RWS.asks $ Map.lookup var
        tp <- case scm of
          Just scm' -> instantiate scm'
          Nothing   -> throwError $ TypeErr pos $ UnboundVar var
        pure (tp, EVar var)
      EApp fun arg -> do
        fun'@Node{ nType = tFun } <- constraint fun
        arg'@Node{ nType = tArg } <- constraint arg
        tRet <- freshVar
        RWS.tell [ Constraint pos tFun $ TFun tArg tRet ]
        pure (tRet, EApp fun' arg')
      EAbs arms -> do
        tArg <- freshVar
        tRet <- freshVar
        arms' <- for arms $ \(Arm pat expr) -> do
          pat'@Node{ nType = tPat } <- constraint pat
          let env = Map.union $ Map.fromList . map (uncurry toScm) $ patVars pat'
          expr'@Node{ nType = tExpr } <- RWS.local env $ constraint expr
          RWS.tell [ Constraint pos tArg tPat
                   , Constraint pos tRet tExpr ]
          pure $ Arm pat' expr'
        pure (TFun tArg tRet, EAbs arms')
      ESeq a b -> do
        a' <- constraint a
        b'@Node { nType = tB } <- constraint b
        pure (tB, ESeq a' b')
      ELet var val expr -> do
        old <- usedVars
        (val'@Node { nType = tVal }, cns) <- RWS.listen $ constraint val
        new <- usedVars
        let scm = generalize cns (new Set.\\ old) tVal
            env = Map.insert var scm
        expr'@Node{ nType = tExpr } <- RWS.local env $ constraint expr
        pure (tExpr, ELet var val' expr')
      EDef var val expr -> do
        old <- usedVars
        tp <- freshVar
        let scm = Forall [] Set.empty tp
            env = Map.insert var scm
        (val', cns) <- RWS.listen $ do
          val'@Node{ nType = tVal } <- RWS.local env $ constraint val
          RWS.tell [ Constraint pos tp tVal ]
          pure val'
        new <- usedVars
        let scm' = generalize cns (new Set.\\ old) tp
            env' = Map.insert var scm'
        expr'@Node { nType = tExpr } <- RWS.local env' $ constraint expr
        pure (tExpr, EDef var val' expr')
    pure $ node{ nType = tp, nKind = kind' }
    where toScm v tp = (v, Forall [] Set.empty tp)

instance Typeable Ast where
  constraint (Ast expr) = Ast <$> constraint expr

unify :: SrcPos -> Type -> Type -> Either TypeError Subst
unify _ (TNat n1) (TNat n2)
  | n1 == n2 = pure mempty
unify _ (TVar v1) (TVar v2)
  | v1 == v2 = pure mempty
unify pos (TVar v) tp
  | occurs v tp = throwError $ TypeErr pos $ OccursError v tp
  | otherwise = pure $ Subst $ Map.singleton v tp
unify pos tp v@TVar{} = unify pos v tp
unify pos (TFun a1 r1) (TFun a2 r2) = unifies [ Constraint pos a1 a2
                                              , Constraint pos r1 r2 ]
unify pos t1 t2 = throwError $ TypeErr pos $ UnifyError t1 t2

unifies :: [Constraint] -> Either TypeError Subst
unifies [] = pure mempty
unifies (Constraint pos a b : cs) = do
  traceShowM ("unify", a, b)
  subs <- unify pos a b
  rest <- unifies $ subst subs cs
  pure $ subs <> rest

infer :: Context -> Ast -> Either TypeError Ast
infer ctx ast = do
  (ast', cns) <- RWS.evalRWST (constraint ast) ctx (Set.empty, 0)
  traceShowM ast'
  traceShowM cns
  subs <- unifies cns
  traceShowM subs
  pure $ subst subs ast'

context :: Context
context = Map.fromList $ map (first Var) ctx
  where ctx = [ ("add", mono $ fun [int, int] int)
              , ("sub", mono $ fun [int, int] int)
              , ("lt", mono $ fun [int, int] bool)
              , ("if", poly ["a"] $ fun [bool, a, a] a)
              , ("println", poly ["a"] $ fun [a] nil)]
        mono = Forall [] Set.empty
        poly = Forall [] . Set.fromList . map TypeVar
        fun ts ret = foldr TFun ret ts
        a = TVar $ TypeVar "a"
        nil = TNat NNil
        int = TNat NInt
        bool = TNat NBool

typeAst :: Ast -> Either TypeError Ast
typeAst = infer context
