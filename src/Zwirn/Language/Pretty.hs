{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Zwirn.Language.Pretty
    ( ppterm
    , ppscheme
    , ppTermHasType
    ) where

{-
    Pretty.hs - pretty printer for the AST and the types
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
import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Constraint

import Prelude hiding ((<>))
import Data.Text (unpack)
import Data.List (intercalate)
import Text.PrettyPrint

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ x = text $ unpack x

instance Pretty Type where
  ppr p (TypeArr a b) = (parensIf (isArrow a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isArrow TypeArr{} = True
      isArrow _ = False
  ppr p (TypeVar a) = ppr p a
  ppr _ (TypeCon a) = text $ unpack a

instance Pretty Predicate where
  ppr p (IsIn c t) = (text $ unpack c) <+> ppr p t

instance Pretty [Predicate] where
  ppr p ps = parensIf (length ps > 1) (hcat (punctuate comma (map (ppr p) ps)))

instance Pretty (Qualified Type) where
  ppr p (Qual [] t) = ppr p t
  ppr p (Qual ps t) = ppr p ps <+> text "=>" <+> ppr p t

instance Pretty Scheme where
  ppr p (Forall _ t) = ppr p t

instance Pretty Term where
  ppr _ (TVar _ x) = text $ unpack x
  ppr _ (TRest) = text "~"
  ppr _ (TText _ x) = text $ unpack x
  ppr _ (TNum _ x) = double $ read $ unpack x
  ppr p (TElong t (Just i))  = (ppr p t) <> text "@" <> int i
  ppr p (TElong t Nothing)   = (ppr p t) <> text "@"
  ppr p (TRepeat t (Just i)) = (ppr p t) <> text "!" <> int i
  ppr p (TRepeat t Nothing)  = (ppr p t) <> text "!"
  ppr p (TSeq ts) = brackets (hcat (punctuate space (map (ppr p) ts)))
  ppr p (TAlt ts) = text "<" <> (hcat (punctuate space (map (ppr p) ts))) <> text ">"
  ppr p (TChoice _ ts) = brackets (hcat $ punctuate (text "|") (map (ppr p) ts))
  ppr p (TStack ts) = brackets (hcat $ punctuate comma (map (ppr p) ts))
  ppr p (TEuclid t1 t2 t3 (Just t4)) = (ppr p t1) <> braces ((ppr p t2) <> comma <> (ppr p t3) <> comma <> (ppr p t4))
  ppr p (TEuclid t1 t2 t3 Nothing) = (ppr p t1) <> braces ((ppr p t2) <> comma <> (ppr p t3))
  ppr p (TPoly t1 t2) = (ppr p t1) <> text "%" <> (ppr p t2)
  ppr p (TApp t1 t2) = parensIf (p > 0) (ppr (p+1) t1 <+>  ppr p t2)
  ppr p (TInfix t1 n t2) = ppr p t1 <+> (text $ unpack n) <+> ppr p t2
  ppr p (TBracket t) = parens (ppr p t)
  ppr p (TLambda vs t) =( text "\\") <> (hcat $ punctuate space $ map (text . unpack) vs) <+> text "->" <+> ppr p t

instance Pretty (Term, Scheme) where
  ppr p (t,s) = ppr p t <+> text "::" <+> ppr p s

pptype :: Type -> String
pptype = render . ppr 0

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

ppterm :: Term -> String
ppterm = render . ppr 0

ppTermHasType :: (Term, Scheme) -> String
ppTermHasType = render . ppr 0

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["Cannot unify types: \n\t", pptype a, " ~ ", pptype b]
  show (UnificationMismatch as bs) =
    concat ["Cannot unify types: \n\t", intercalate "," $ map pptype as, " ~ ", intercalate "," $ map pptype bs]
  show (InfiniteType a b) =
    concat ["Cannot construct the infinite type: ", unpack a, " = ", pptype b]
  show (Ambigious cs) =
    concat ["Cannot not match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a,b) <- cs]
  show (UnboundVariable a) = "Not in scope: " ++ unpack a
  show (NoInstance (IsIn c x)) = "No instance for " ++ unpack c ++ " " ++ pptype x
