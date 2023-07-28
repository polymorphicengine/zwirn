{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Simple
    ( simplify
    , simplifyDef
    , SimpleTerm (..)
    , SimpleDef (..)
    , Position (..)
    ) where

import Zwirn.Language.Syntax

import Data.Text (pack)

-- simple representation of patterns
data SimpleTerm
  = SVar (Maybe Position) Var
  | SRest
  | SElong SimpleTerm Int
  | SSeq [SimpleTerm]
  | SStack [SimpleTerm]
  | SChoice Int [SimpleTerm]
  | SEuclid SimpleTerm SimpleTerm SimpleTerm (Maybe SimpleTerm)
  | SLambda Var SimpleTerm
  | SApp SimpleTerm SimpleTerm
  | SInfix SimpleTerm OperatorSymbol SimpleTerm
  deriving (Eq, Show)

data SimpleDef
  = LetS Var SimpleTerm
  deriving (Eq, Show)

simplify :: Term -> SimpleTerm
simplify (TVar p x) = SVar (Just p) x
simplify (TRest) = SRest
simplify x@(TElong _ _) = case getTotalElong x of
                                TElong t (Just i) -> SElong (simplify t) i
                                TElong t Nothing -> SElong (simplify t) 2
                                t -> simplify t
simplify x@(TRepeat _ _) = SSeq $ map simplify $ resolveRepeat x
simplify (TSeq ts) = SSeq (map simplify $ concatMap resolveRepeat ts)
simplify (TStack ts) = SStack (map simplify ts)
simplify (TChoice i ts) = SChoice i (map simplify ts)
simplify (TAlt ts) = SInfix (SSeq ss) "/" (SVar Nothing (pack $ show $ length ss))
                   where ss = map simplify ts
simplify (TEuclid t1 t2 t3 (Just t4)) = SEuclid (simplify t1) (simplify t2) (simplify t3) (Just $ simplify t4)
simplify (TEuclid t1 t2 t3 Nothing) = SEuclid (simplify t1) (simplify t2) (simplify t3) Nothing
simplify (TPoly (TSeq ts) n) = SInfix (SInfix (SSeq ss) "/" (SVar Nothing (pack $ show $ length ss))) "*" (simplify n)
                   where ss = map simplify ts
simplify (TPoly x n) = SInfix (simplify x) "*" (simplify n)
simplify (TLambda [] t) = simplify t
simplify (TLambda (x:xs) t) = SLambda x (simplify $ TLambda xs t)
simplify (TApp x y) = SApp (simplify x) (simplify y)
simplify (TInfix x op y) = SInfix (simplify x) op (simplify y)

simplifyDef :: Def -> SimpleDef
simplifyDef (Let x vs t) = LetS x (simplify $ TLambda vs t)

resolveRepeat :: Term -> [Term]
resolveRepeat t = case getTotalRepeat t of
                          TRepeat x (Just i) -> replicate i x
                          TRepeat x Nothing -> [x,x]
                          x -> [x]


--TODO : not completely right when Nothing followed by Just...
getRepeat :: (Term, Int) -> Term
getRepeat ((TRepeat x (Just j)), k) = getRepeat (x,j*k)
getRepeat ((TRepeat x Nothing), k) = getRepeat (x, k+1)
getRepeat (x, j) = TRepeat x (Just j)

getTotalRepeat :: Term -> Term
getTotalRepeat (TRepeat t (Just i)) = getRepeat (t,i)
getTotalRepeat (TRepeat t Nothing) = getRepeat (t,2)
getTotalRepeat t = t

getElong :: (Term, Int) -> Term
getElong ((TElong x (Just j)), k) = getElong (x,j*k)
getElong ((TElong x Nothing), k) = getElong (x, k+1)
getElong (x, j) = TElong x (Just j)

getTotalElong :: Term -> Term
getTotalElong (TElong t (Just i)) = getElong (t,i)
getTotalElong (TElong t Nothing) = getElong (t,2)
getTotalElong t = t
