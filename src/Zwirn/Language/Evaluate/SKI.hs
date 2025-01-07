{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.SKI (evaluate, (!)) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Zwirn.Core.Cord
import Zwirn.Core.Core
import Zwirn.Core.Random (chooseWithSeed)
import Zwirn.Language.Evaluate.Convert
import Zwirn.Language.Evaluate.Expression
import Zwirn.Language.Simple
import Zwirn.Language.TypeCheck.Types

compile :: SimpleTerm -> Expression
compile (SVar _ n) = EVar n
compile (SApp fun arg) = EApp (compile fun) (compile arg)
compile (SLambda x body) = abstract x (compile body)
compile (SNum _ x) = EZwirn $ pure $ ENum $ read $ unpack x
compile (SText _ x) = EZwirn $ pure $ EText x
compile (SSeq xs) = ESeq $ map compile xs
compile (SStack xs) = EStack $ map compile xs
compile (SChoice i xs) = EChoice i $ map compile xs
compile (SInfix s1 n s2) = EApp (EApp (EVar n) (compile s1)) (compile s2)
compile (SBracket s) = compile s
compile SRest = EZwirn silence
compile _ = error "not yet implemented"

abstract :: Name -> Expression -> Expression
abstract x (EVar n) | x == n = combI
abstract x (EApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (ESeq xs) = ESeq $ map (abstract x) xs
abstract x (EStack xs) = EStack $ map (abstract x) xs
abstract x (EChoice i xs) = EChoice i $ map (abstract x) xs
abstract _ k = combK k

combS :: Expression -> Expression -> Expression
combS f = EApp (EApp (EVar "scomb") f)

combK :: Expression -> Expression
combK = EApp (EVar "const")

combI :: Expression
combI = EVar "id"

infixl 0 !

(!) :: Expression -> Expression -> Expression
(EZwirn fp) ! (EZwirn x) = EZwirn $ squeezeApply (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) x
_ ! _ = error "Error in (!)"

link :: ExpressionMap -> Expression -> Expression
link bs (EVar n) = fromJust (Map.lookup n bs)
link bs (EApp f x) = link bs f ! link bs x
link bs (ESeq xs) = EZwirn $ fastcat $ map (toZwirn . link bs) xs
link bs (EStack xs) = EZwirn $ stack $ map (toZwirn . link bs) xs
link bs (EChoice i xs) = EZwirn $ chooseWithSeed i $ map (toZwirn . link bs) xs
link _ e = e

evaluate :: ExpressionMap -> SimpleTerm -> Expression
evaluate bs = link bs . compile
