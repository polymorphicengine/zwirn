{-# LANGUAGE OverloadedStrings #-}

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

numberT :: Type
numberT = TypeCon "Number"

textT :: Type
textT = TypeCon "Text"

mapT :: Type
mapT = TypeCon "Map"

varA :: Type
varA = TypeVar "a"

varB :: Type
varB = TypeVar "b"

varC :: Type
varC = TypeVar "c"

isBasicType :: Scheme -> Bool
isBasicType (Forall [] (Qual [] (TypeCon _))) = True
isBasicType (Forall _ (Qual [] (TypeVar _))) = True
isBasicType _ = False

infixr 1 -->

(-->) :: Type -> Type -> Type
(-->) = TypeArr

unqual :: Type -> Qualified Type
unqual = Qual []
