{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module Zwirn.Language.TypeCheck.Types where

{-
    Types.hs - defintion of types adapted from
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

import Data.Text (Text)

type Name = Text
type TypeVar = Text

data Type
  = TypeVar TypeVar
  | TypeCon Text
  | TypeArr Type Type
  deriving (Show, Eq, Ord)

data Predicate
  = IsIn Name Type
  deriving (Show, Eq, Ord)

data Qualified t
 = Qual [Predicate] t
 deriving (Show, Eq, Ord)

data Scheme
 = Forall [TypeVar] (Qualified Type)
 deriving (Show, Eq)

type Instance = Predicate

pattern Number :: Scheme
pattern Number =  (Forall [] (Qual [] (TypeCon "Number")))

pattern Text :: Scheme
pattern Text = (Forall [] (Qual [] (TypeCon "Text")))

pattern ValueMap :: Scheme
pattern ValueMap = (Forall [] (Qual [] (TypeCon "ValueMap")))

pattern Var :: Text -> Scheme
pattern Var x = (Forall [] (Qual [] (TypeVar x)))

numberT :: Type
numberT = TypeCon "Number"

textT :: Type
textT = TypeCon "Text"

valMapT :: Type
valMapT = TypeCon "ValueMap"

isBasicType :: Scheme -> Bool
isBasicType (Forall [] (Qual [] (TypeCon _))) = True
isBasicType _ = False

filterPatClass :: Scheme -> Scheme
filterPatClass (Forall xs (Qual ps t)) = Forall xs (Qual fs t)
                                       where fs = filter isNotPatClass ps
                                             isNotPatClass (IsIn "Pat" _) = False
                                             isNotPatClass _ = True
