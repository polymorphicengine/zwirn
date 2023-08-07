{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.TypeCheck.Types where

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
