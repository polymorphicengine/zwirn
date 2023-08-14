{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Zwirn.Language.TypeCheck.Constraint
    ( Substitutable (..)
    , Subst (..)
    , TypeError (..)
    , Constraint
    , runSolve
    ) where

{-
    Constraint.hs - unification constraint solver adapted from
    https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter7/poly_constraints
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Env as Env


import Control.Monad.Except
import Control.Monad.Identity

import           Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Set as Set

data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVar Type
  | UnboundVariable Text
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | NoInstance Predicate
  deriving (Show, Eq)


type Constraint = (Type, Type)

newtype Subst = Subst (Map.Map TypeVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TypeVar

instance Substitutable Type where
  apply _ (TypeCon a)       = TypeCon a
  apply (Subst s) t@(TypeVar a) = Map.findWithDefault t a s
  apply s (t1 `TypeArr` t2) = apply s t1 `TypeArr` apply s t2

  ftv TypeCon{}         = Set.empty
  ftv (TypeVar a)       = Set.singleton a
  ftv (t1 `TypeArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t)   = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv ty cl) = TypeEnv (Map.map (apply s) ty) (apply s cl)
  ftv (TypeEnv ty cl) = (ftv $ Map.elems ty) `Set.union` (ftv cl)

instance Substitutable Predicate where
  apply s (IsIn x t) = IsIn x (apply s t)
  ftv (IsIn _ t) = ftv t

instance Substitutable t => Substitutable (Qualified t) where
  apply s (Qual ps t) = Qual (apply s ps) (apply s t)
  ftv (Qual ps t) = ftv ps `Set.union`ftv t


-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TypeVar v) t = v `bind` t
unifies t (TypeVar v) = v `bind` t
unifies (TypeArr t1 t2) (TypeArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind ::  TypeVar -> Type -> Solve Subst
bind a t | t == TypeVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck ::  Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
