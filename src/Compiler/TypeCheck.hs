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
import           Control.Monad.Except (throwError)
import qualified Control.Monad.RWS as RWS
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Traversable (for)

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
  Subst a <> subs@(Subst b) = Subst $ b <> Map.map (subst subs) a

instance Monoid Subst where
  mempty = Subst mempty

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
  subst subs = runIdentity . preAst sub sub
    where sub n@Node{ nType = tp } = pure n{ nType = subst subs tp }

instance (Substitutable a) => Substitutable [a] where
  subst = map . subst

freshVar :: Infer Type
freshVar = do
  (vs, i) <- RWS.get
  let var = TypeVar $ 't' : show i
  RWS.put (Set.insert var vs, i + 1)
  pure $ TVar $ var

usedVars :: Infer (Set.Set TypeVar)
usedVars = RWS.gets fst

withVar :: Var -> Scheme -> Infer a -> Infer a
withVar var scm = RWS.local $ Map.insert var scm

withVars :: Map.Map Var Scheme -> Infer a -> Infer a
withVars = RWS.local . (<>)

freeVars :: Type -> Set.Set TypeVar
freeVars tp = case tp of
  TNat{}   -> mempty
  TVar var -> Set.singleton var
  TFun p r -> freeVars p <> freeVars r

occurs :: TypeVar -> Type -> Bool
occurs tv a = tv `Set.member` freeVars a

mono :: Type -> Scheme
mono = Forall [] mempty

instantiate :: Scheme -> Infer Type
instantiate (Forall cns tvs tp) = do
  subs <- Subst <$> sequenceA (Map.fromSet (const freshVar) tvs)
  RWS.tell $ subst subs cns
  pure $ subst subs tp

generalize :: Infer (Node a) -> Infer (Node a, Scheme)
generalize inf = do
  old <- usedVars
  (a@Node { nType = tp }, cns) <- RWS.listen inf
  new <- usedVars
  let tvs = new Set.\\ old
      scm = Forall cns tvs tp
  pure (a, scm)

patVars :: Pat -> Map.Map Var Type
patVars Node{ nType = tp, nKind = kind } = case kind of
  PLit{} -> mempty
  PVar v -> Map.singleton v tp
  PIgn   -> mempty

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
          let vars = mono <$> patVars pat'
          expr'@Node{ nType = tExpr } <- withVars vars $ constraint expr
          RWS.tell [ Constraint pos tArg tPat
                   , Constraint pos tRet tExpr ]
          pure $ Arm pat' expr'
        pure (TFun tArg tRet, EAbs arms')
      ESeq a b -> do
        a' <- constraint a
        b'@Node { nType = tB } <- constraint b
        pure (tB, ESeq a' b')
      ELet var val expr -> do
        (val', scm) <- generalize $ constraint val
        expr'@Node{ nType = tExpr } <- withVar var scm $ constraint expr
        pure (tExpr, ELet var val' expr')
      EDef var val expr -> do
        (val', scm) <- generalize $ do
          tp <- freshVar
          let scm = mono tp
          val'@Node{ nType = tVal } <- withVar var scm $ constraint val
          RWS.tell [ Constraint pos tp tVal ]
          pure val'
        expr'@Node { nType = tExpr } <- withVar var scm $ constraint expr
        pure (tExpr, EDef var val' expr')
    pure $ node{ nType = tp, nKind = kind' }

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
  subs <- unify pos a b
  rest <- unifies $ subst subs cs
  pure $ subs <> rest

infer :: Context -> Ast -> Either TypeError Ast
infer ctx ast = do
  (ast', cns) <- RWS.evalRWST (constraint ast) ctx (Set.empty, 0)
  subs <- unifies cns
  pure $ subst subs ast'

context :: Context
context = Map.fromList $ map (first Var) ctx
  where ctx = [ ("add", mono $ fun [int, int] int)
              , ("sub", mono $ fun [int, int] int)
              , ("lt", mono $ fun [int, int] bool)
              , ("if", poly ["a"] $ fun [bool, a, a] a)
              , ("println", poly ["a"] $ fun [a] nil)]
        poly = Forall [] . Set.fromList . map TypeVar
        fun = flip $ foldr TFun
        a = TVar $ TypeVar "a"
        nil = TNat NNil
        int = TNat NInt
        bool = TNat NBool

typeAst :: Ast -> Either TypeError Ast
typeAst = infer context
