module Compiler.Type
  ( TypeVar(..)
  , Native(..)
  , Type(..)
  , Constraint(..)
  , Scheme(..)
  ) where

import           Compiler.Data
import qualified Data.Set as Set

newtype TypeVar = TypeVar String
                deriving (Eq, Ord)

data Native = NNil
            | NInt
            | NStr
            | NBool
            deriving (Eq)

data Type = TNat Native
          | TVar TypeVar
          | TFun Type Type

data Constraint = Constraint SrcPos Type Type

data Scheme = Forall [Constraint] (Set.Set TypeVar) Type

instance Show TypeVar where
  show (TypeVar v) = v

instance Show Native where
  show NNil  = "Nil"
  show NInt  = "Int"
  show NStr  = "Str"
  show NBool = "Bool"

instance Show Type where
  show (TNat nat) = show nat
  show (TVar var) = show var
  show (TFun p r) = "(" ++ show p ++ " -> " ++ show r ++ ")"

instance Show Constraint where
  show (Constraint _ t1 t2) = show t1 ++ " = " ++ show t2
