{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Simple
    ( simplify
    , simplifyDef
    , insertFixpoint
    , SimpleTerm (..)
    , SimpleDef (..)
    , Position (..)
    ) where

{-
    Simple.hs - desugaring of the zwirn AST
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

import Zwirn.Language.Syntax

import Data.Text (Text,pack)

-- simple representation of patterns
data SimpleTerm
  = SVar Position Var
  | SText Position Text
  | SNum (Maybe Position) Text
  | SRest
  | SElong SimpleTerm Int
  | SSeq [SimpleTerm]
  | SStack [SimpleTerm]
  | SChoice Int [SimpleTerm]
  | SEuclid SimpleTerm SimpleTerm SimpleTerm (Maybe SimpleTerm)
  | SLambda Var SimpleTerm
  | SApp SimpleTerm SimpleTerm
  | SInfix SimpleTerm OperatorSymbol SimpleTerm
  | SBracket SimpleTerm
  deriving (Eq, Show)

data SimpleDef
  = LetS Var SimpleTerm
  deriving (Eq, Show)

simplify :: Term -> SimpleTerm
simplify (TVar p x) = SVar p x
simplify (TText p x) = SText p x
simplify (TNum p x) = SNum (Just p) x
simplify (TRest) = SRest
simplify x@(TElong _ _) = case getTotalElong x of
                                TElong t (Just i) -> SElong (simplify t) i
                                TElong t Nothing -> SElong (simplify t) 2
                                t -> simplify t
simplify x@(TRepeat _ _) = SSeq $ map simplify $ resolveRepeat x
simplify (TSeq ts) = SSeq (map simplify $ concatMap resolveRepeat ts)
simplify (TStack ts) = SStack (map simplify ts)
simplify (TChoice i ts) = SChoice i (map simplify ts)
simplify (TAlt ts) = SBracket $ SInfix (SSeq ss) "/" (SNum Nothing (pack $ show $ length ss))
                   where ss = (map simplify $ concatMap resolveRepeat ts)
simplify (TEuclid t1 t2 t3 (Just t4)) = SEuclid (simplify t1) (simplify t2) (simplify t3) (Just $ simplify t4)
simplify (TEuclid t1 t2 t3 Nothing) = SEuclid (simplify t1) (simplify t2) (simplify t3) Nothing
simplify (TPoly (TSeq ts) n) = SBracket $ SInfix (SInfix (SSeq ss) "/" (SNum Nothing (pack $ show $ length ss))) "*" (simplify n)
                   where ss = (map simplify $ concatMap resolveRepeat ts)
simplify (TPoly x n) = SInfix (simplify x) "*" (simplify n)
simplify (TLambda [] t) = simplify t
simplify (TLambda (x:xs) t) = SLambda x (simplify $ TLambda xs t)
simplify (TApp x y) = SApp (simplify x) (simplify y)
simplify (TInfix x op y) = SInfix (simplify x) op (simplify y)
simplify (TBracket x) = SBracket (simplify x)

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

insertFixpoint :: SimpleDef -> SimpleDef
insertFixpoint d@(LetS x t) = case occurs x t of
                                 True -> LetS x (SApp (SVar (Pos 0 0 0 0) "fixpoint") (SLambda x t))
                                 False -> d
                            where occurs v (SVar _ y) = v == y
                                  occurs _ (SNum _ _) = False
                                  occurs _ (SText _ _ ) = False
                                  occurs _ SRest = False
                                  occurs v (SElong tt _) = occurs v tt
                                  occurs v (SSeq ts) = or $ map (occurs v) ts
                                  occurs v (SStack ts) = or $ map (occurs v) ts
                                  occurs v (SChoice _ ts) = or $ map (occurs v) ts
                                  occurs v (SEuclid t1 t2 t3 Nothing) = or $ map (occurs v) [t1, t2, t3]
                                  occurs v (SEuclid t1 t2 t3 (Just t4)) = or $ map (occurs v) [t1, t2, t3, t4]
                                  occurs v (SLambda y tt) = case v == y of
                                                              True -> False
                                                              False -> occurs v tt
                                  occurs v (SApp t1 t2) = occurs v t1 || occurs v t2
                                  occurs v (SInfix t1 _ t2) = occurs v t1 || occurs v t2
                                  occurs v (SBracket tt) = occurs v tt
