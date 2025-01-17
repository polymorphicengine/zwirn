{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.SKI (evaluate, (!)) where

{-
    SKI.hs - evaluate epxressions via the SKI combinator calculus,
    code adapted from https://kseo.github.io/posts/2016-12-30-write-you-an-interpreter.html
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

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Zwirn.Core.Cord
import Zwirn.Core.Core
import Zwirn.Core.Modulate
import Zwirn.Core.Random (chooseWithSeed)
import Zwirn.Core.Types
import Zwirn.Language.Evaluate.Convert
import Zwirn.Language.Evaluate.Expression
import Zwirn.Language.Simple
import Zwirn.Language.TypeCheck.Types

compile :: SimpleTerm -> Expression
compile (SVar p n) = EVar p n
compile (SApp fun arg) = EApp (compile fun) (compile arg)
compile (SLambda x body) = abstract x (compile body)
compile (SNum (Just p) x) = EZwirn $ addInfo p $ pure $ ENum $ read $ unpack x
compile (SNum Nothing x) = EZwirn $ pure $ ENum $ read $ unpack x
compile (SText p x) = EZwirn $ addInfo p $ pure $ EText x
compile (SSeq xs) = ESeq $ map compile xs
compile (SStack xs) = EStack $ map compile xs
compile (SChoice i xs) = EChoice i $ map compile xs
compile (SInfix s1 n s2) = EApp (EApp (EVar Nothing n) (compile s1)) (compile s2)
compile (SBracket s) = compile s
compile SRest = EZwirn silence
compile _ = error "not yet implemented"

abstract :: Name -> Expression -> Expression
abstract x (EVar _ n) | x == n = combI
abstract x (EApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (ESeq xs) = ESeq $ map (abstract x) xs
abstract x (EStack xs) = EStack $ map (abstract x) xs
abstract x (EChoice i xs) = EChoice i $ map (abstract x) xs
abstract _ k = combK k

combS :: Expression -> Expression -> Expression
combS f = EApp (EApp (EVar Nothing "scomb") f)

combK :: Expression -> Expression
combK = EApp (EVar Nothing "const")

combI :: Expression
combI = EVar Nothing "id"

infixl 0 !

(!) :: Expression -> Expression -> Expression
(EZwirn fp) ! (EZwirn x) = EZwirn $ squeezeApply (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) x
_ ! _ = error "Error in (!)"

link :: ExpressionMap -> Expression -> Expression
link bs (EVar (Just p) n) = addPosExp p $ fromJust (Map.lookup n bs)
link bs (EVar Nothing n) = fromJust (Map.lookup n bs)
link bs (EApp f x) = link bs f ! link bs x
link bs (ESeq xs) = EZwirn $ fastcat $ map (toZwirn . link bs) xs
link bs (EStack xs) = EZwirn $ stack $ map (toZwirn . link bs) xs
link bs (EChoice i xs) = EZwirn $ chooseWithSeed i $ map (toZwirn . link bs) xs
link _ e = e

evaluate :: ExpressionMap -> SimpleTerm -> Expression
evaluate bs = link bs . compile

addPosExp :: Position -> Expression -> Expression
addPosExp p (EZwirn x) = EZwirn $ withInfos (p :) x
addPosExp _ x = x
