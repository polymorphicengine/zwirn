{-# LANGUAGE OverloadedStrings #-}

module Zwirn.Language.Simple
  ( simplify,
    simplifyDef,
    SimpleTerm (..),
    SimpleDef (..),
    Position (..),
  )
where

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

import Data.Bifunctor (second)
import Data.Text as Text (Text, filter, pack)
import Zwirn.Language.Syntax

-- simple representation of patterns
data SimpleTerm
  = SVar (Maybe Position) Var
  | SText Position Text
  | SNum (Maybe Position) Text
  | SRest
  | SSeq [SimpleTerm]
  | SStack [SimpleTerm]
  | SChoice Int [SimpleTerm]
  | SCase SimpleTerm (Maybe SimpleTerm) [(Pattern, SimpleTerm)]
  | SLambda Var SimpleTerm
  | SApp SimpleTerm SimpleTerm
  | SInfix SimpleTerm OperatorSymbol SimpleTerm
  | SBracket SimpleTerm
  deriving (Eq, Show)

data SimpleDef
  = LetS Var SimpleTerm
  deriving (Eq, Show)

simplify :: Term -> SimpleTerm
simplify (TVar p x) = SVar (Just p) x
simplify (TText p x) = SText p $ stripText x
  where
    stripText = Text.filter (/= '\"')
simplify (TNum p x) = SNum (Just p) x
simplify TRest = SRest
simplify x@(TRepeat _ _) = SSeq $ map simplify $ resolveRepeat x
simplify (TSeq ts) = SSeq (map simplify $ concatMap resolveRepeat ts)
simplify (TStack ts) = SStack (map simplify ts)
simplify (TChoice i ts) = SChoice i (map simplify ts)
simplify (TCase x def xs) = SCase (simplify x) (fmap simplify def) (map (second simplify) xs)
simplify (TAlt ts) = SBracket $ SInfix (SSeq ss) "/" (SNum Nothing (pack $ show $ length ss))
  where
    ss = map simplify $ concatMap resolveRepeat ts
simplify (TPoly (TSeq ts) n) = SBracket $ SInfix (SInfix (SSeq ss) "/" (SNum Nothing (pack $ show $ length ss))) "*" (simplify n)
  where
    ss = map simplify $ concatMap resolveRepeat ts
simplify (TPoly x n) = SInfix (simplify x) "*" (simplify n)
simplify (TLambda [] t) = simplify t
simplify (TLambda (x : xs) t) = SLambda x (simplify $ TLambda xs t)
simplify (TIfThenElse x y (Just z)) = SApp (SApp (SApp (SVar Nothing "ifthen") (simplify x)) (simplify y)) (simplify z)
simplify (TIfThenElse x y Nothing) = SApp (SApp (SVar Nothing "if") (simplify x)) (simplify y)
simplify (TApp x y) = SApp (simplify x) (simplify y)
simplify (TInfix x op y) = SInfix (simplify x) op (simplify y)
simplify (TSectionR op y) = SLambda "_x" (SInfix (SVar Nothing "_x") op (simplify y))
simplify (TSectionL x op) = SLambda "_x" (SInfix (simplify x) op (SVar Nothing "_x"))
simplify (TBracket x) = SBracket (simplify x)
simplify (TEnum Run x y) = SApp (SApp (SVar Nothing "runFromTo") (simplify x)) (simplify y)
simplify (TEnumThen Run x y z) = SApp (SApp (SApp (SVar Nothing "runFromThenTo") (simplify x)) (simplify y)) (simplify z)
simplify (TEnum Alt x y) = SApp (SApp (SVar Nothing "slowrunFromTo") (simplify x)) (simplify y)
simplify (TEnumThen Alt x y z) = SApp (SApp (SApp (SVar Nothing "slowrunFromThenTo") (simplify x)) (simplify y)) (simplify z)
simplify (TEnum Cord x y) = SApp (SApp (SVar Nothing "cordFromTo") (simplify x)) (simplify y)
simplify (TEnumThen Cord x y z) = SApp (SApp (SApp (SVar Nothing "cordFromThenTo") (simplify x)) (simplify y)) (simplify z)
simplify (TEnum Choice x y) = SApp (SApp (SVar Nothing "chooseFromTo") (simplify x)) (simplify y)
simplify (TEnumThen Choice x y z) = SApp (SApp (SApp (SVar Nothing "chooseFromThenTo") (simplify x)) (simplify y)) (simplify z)

simplifyDef :: Def -> SimpleDef
simplifyDef (Let x vs t) = LetS x (simplify $ TLambda vs t)

resolveRepeat :: Term -> [Term]
resolveRepeat t = case getTotalRepeat t of
  TRepeat x (Just i) -> replicate i x
  TRepeat x Nothing -> [x, x]
  x -> [x]

-- TODO : not completely right when Nothing followed by Just...
getRepeat :: (Term, Int) -> Term
getRepeat (TRepeat x (Just j), k) = getRepeat (x, j * k)
getRepeat (TRepeat x Nothing, k) = getRepeat (x, k + 1)
getRepeat (x, j) = TRepeat x (Just j)

getTotalRepeat :: Term -> Term
getTotalRepeat (TRepeat t (Just i)) = getRepeat (t, i)
getTotalRepeat (TRepeat t Nothing) = getRepeat (t, 2)
getTotalRepeat t = t
