{-# LANGUAGE OverloadedStrings #-}

module Zwirn.Language.TypeCheck.Infer
  ( inferTerm,
    generalize,
  )
where

{-
    Infer.hs - type inference algorithm adapted from
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

import Control.Monad (replicateM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
-- import Zwirn.Language.TypeCheck.Env as Env

import Zwirn.Language.Environment
import Zwirn.Language.Simple
import Zwirn.Language.TypeCheck.Constraint
import Zwirn.Language.TypeCheck.Types

-- | Inference monad
type Infer a =
  ( ReaderT
      InterpreterEnv -- Typing environment
      ( StateT -- Inference state
          InferState
          ( Except -- Inference errors
              TypeError
          )
      )
      a -- Result
  )

-- | Inference state
newtype InferState = InferState {count :: Int}

-- | Initial inference state
initInfer :: InferState
initInfer = InferState {count = 0}

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: InterpreterEnv -> Infer a -> Either TypeError a
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferTerm :: InterpreterEnv -> SimpleTerm -> Either TypeError Scheme
inferTerm env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, ps, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> case runInfer env (filterAndCheck (apply subst ps) (apply subst ty)) of
      Left err -> Left err
      Right xs -> Right $ closeOver xs $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
-- constraintsTerm :: Env -> SimpleTerm -> Either TypeError ([Constraint], Subst, Type, Scheme)
-- constraintsTerm env ex = case runInfer env (infer ex) of
--   Left err -> Left err
--   Right (ty, cs) -> case runSolve cs of
--     Left err -> Left err
--     Right subst -> Right (cs, subst, ty, sc)
--       where
--         sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: [Predicate] -> Type -> Scheme
closeOver ps t = normalize $ generalize ps t

-- | modified environment where x :: sc
inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope = insertType x sc
  local scope m

-- | Lookup type in the environment
lookupEnv :: Name -> Infer (Type, [Predicate])
lookupEnv x = do
  env <- ask
  case lookupType x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

letters :: [Text]
letters = map pack $ [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TypeVar (letters !! count s)

instantiate :: Scheme -> Infer (Type, [Predicate])
instantiate (Forall as (Qual ps t)) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ (apply s t, apply s ps)

generalize :: [Predicate] -> Type -> Scheme
generalize ps t = Forall as (Qual ps t)
  where
    as = Set.toList $ ftv t

filterAndCheck :: [Predicate] -> Type -> Infer [Predicate]
filterAndCheck [] _ = return []
filterAndCheck (p@(IsIn _ (TypeVar _)) : ps) t =
  if or $ Set.map (\x -> elem x $ ftv p) (ftv t)
    then (p :) <$> filterAndCheck ps t
    else filterAndCheck ps t
filterAndCheck (p : ps) t = checkInstance p >> filterAndCheck ps t

checkInstance :: Predicate -> Infer ()
checkInstance p = do
  (IEnv _ is) <- ask
  (if p `elem` is then return () else throwError $ NoInstance p)

infer :: SimpleTerm -> Infer (Type, [Predicate], [Constraint])
infer expr = case expr of
  SVar _ x -> do
    (t, ps) <- lookupEnv x
    return (t, ps, [])
  SText _ _ -> return (textT, [], [])
  SNum _ _ -> return (numberT, [], [])
  SBracket s -> infer s
  SRest -> do
    tv <- fresh
    return (tv, [], [])
  SLambda x e -> do
    tv <- fresh
    (t, ps, c) <- inEnv (x, Forall [] (Qual [] tv)) (infer e)
    return (tv `TypeArr` t, ps, c)
  SApp e1 e2 -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    tv <- fresh
    return (tv, ps1 ++ ps2, c1 ++ c2 ++ [(t1, t2 `TypeArr` tv)])
  SInfix e1 op e2 -> do
    (t1, ps1, c1) <- infer e1
    (t2, ps2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TypeArr` (t2 `TypeArr` tv)
    (u2, p3) <- lookupEnv op
    return (tv, ps1 ++ ps2 ++ p3, c1 ++ c2 ++ [(u1, u2)])
  SSeq (x : xs) -> do
    (t, ps, cs) <- infer x
    infs <- mapM infer xs
    return (t, ps, cs ++ concatMap (\(_, _, y) -> y) infs ++ [(t, t') | t' <- map (\(y, _, _) -> y) infs])
  SStack (x : xs) -> do
    (t, ps, cs) <- infer x
    infs <- mapM infer xs
    return (t, ps, cs ++ concatMap (\(_, _, y) -> y) infs ++ [(t, t') | t' <- map (\(y, _, _) -> y) infs])
  SChoice _ (x : xs) -> do
    (t, ps, cs) <- infer x
    infs <- mapM infer xs
    return (t, ps, cs ++ concatMap (\(_, _, y) -> y) infs ++ [(t, t') | t' <- map (\(y, _, _) -> y) infs])
  _ -> error "Can't happen"

normalize :: Scheme -> Scheme
normalize (Forall _ (Qual ps body)) = Forall (map snd ord) (Qual (map normpred ps) $ normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (TypeVar a) = [a]
    fv (TypeArr a b) = fv a ++ fv b
    fv (TypeCon _) = []

    normtype (TypeArr a b) = TypeArr (normtype a) (normtype b)
    normtype (TypeCon a) = TypeCon a
    normtype (TypeVar a) =
      case Prelude.lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"

    normpred (IsIn n t) = IsIn n (normtype t)
