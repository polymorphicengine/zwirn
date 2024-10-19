{-# LANGUAGE OverloadedStrings #-}

module Zwirn.Language.Default
  ( defaultTypeEnv,
  )
where

{-
    Default.hs - provides the default type environment,
    automatically generated
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

import Data.Map as Map
import Data.Text (Text)
import Zwirn.Language.TypeCheck.Env
import Zwirn.Language.TypeCheck.Types

defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv (Map.fromList $ others ++ typeList) defaultInstances

defaultInstances :: [Instance]
defaultInstances =
  [ IsIn "Num" numberT,
    IsIn "Num" valMapT,
    IsIn "Fractional" numberT,
    IsIn "Fractional" valMapT,
    IsIn "Ord" numberT,
    IsIn "Ord" valMapT,
    IsIn "Show" numberT,
    IsIn "Show" valMapT,
    IsIn "show" textT
  ]

others :: [(Text, Scheme)]
others =
  [ ("\'", Forall ["a", "b"] (Qual [] (TypeArr (TypeVar "a") (TypeArr (TypeArr (TypeVar "a") (TypeVar "b")) (TypeVar "b")))))
  ]

typeList :: [(Text, Scheme)]
typeList = [("$", Forall ["a", "b"] (Qual [] (TypeArr (TypeArr (TypeVar "a") (TypeVar "b")) (TypeArr (TypeVar "a") (TypeVar "b"))))), ("&", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number"))))), ("*", Forall ["a"] (Qual [] (TypeArr (TypeVar "a") (TypeArr (TypeCon "Number") (TypeVar "a"))))), ("+", Forall ["a"] (Qual [IsIn "Num" (TypeVar "a")] (TypeArr (TypeVar "a") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("++", Forall [] (Qual [] (TypeArr (TypeCon "Text") (TypeArr (TypeCon "Text") (TypeCon "Text"))))), ("-", Forall ["a"] (Qual [IsIn "Num" (TypeVar "a")] (TypeArr (TypeVar "a") (TypeArr (TypeVar "a") (TypeVar "a"))))), (".", Forall ["a", "b", "d"] (Qual [] (TypeArr (TypeArr (TypeVar "b") (TypeVar "d")) (TypeArr (TypeArr (TypeVar "a") (TypeVar "b")) (TypeArr (TypeVar "a") (TypeVar "d")))))), (".|.", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number"))))), ("/", Forall ["a"] (Qual [] (TypeArr (TypeVar "a") (TypeArr (TypeCon "Number") (TypeVar "a"))))), ("//", Forall ["a"] (Qual [IsIn "Fractional" (TypeVar "a")] (TypeArr (TypeVar "a") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("<<", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number"))))), ("<=", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number"))))), ("<~", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("==", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number"))))), (">=", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number"))))), (">>", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number"))))), ("ascii", Forall [] (Qual [] (TypeArr (TypeCon "Text") (TypeCon "Number")))), ("bd", Forall [] (Qual [] (TypeCon "Text"))), ("bytebeat", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeCon "Number")))), ("const", Forall ["a", "b"] (Qual [] (TypeArr (TypeVar "a") (TypeArr (TypeVar "b") (TypeVar "a"))))), ("cyc", Forall [] (Qual [] (TypeCon "Number"))), ("false", Forall [] (Qual [] (TypeCon "Number"))), ("fast", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("floor", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeCon "Number")))), ("id", Forall ["a"] (Qual [] (TypeArr (TypeVar "a") (TypeVar "a")))), ("irand", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeCon "Number")))), ("now", Forall [] (Qual [] (TypeCon "Number"))), ("ply", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("rand", Forall [] (Qual [] (TypeCon "Number"))), ("range", Forall [] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeCon "Number")))))), ("rev", Forall ["a"] (Qual [] (TypeArr (TypeVar "a") (TypeVar "a")))), ("rotL", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("rotR", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("saw", Forall [] (Qual [] (TypeCon "Number"))), ("segment", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("show", Forall ["a"] (Qual [IsIn "Show" (TypeVar "a")] (TypeArr (TypeVar "a") (TypeCon "Text")))), ("sine", Forall [] (Qual [] (TypeCon "Number"))), ("slow", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("sn", Forall [] (Qual [] (TypeCon "Text"))), ("somecycles", Forall ["a"] (Qual [] (TypeArr (TypeArr (TypeVar "a") (TypeVar "a")) (TypeArr (TypeVar "a") (TypeVar "a"))))), ("somecyclesBy", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeArr (TypeVar "a") (TypeVar "a")) (TypeArr (TypeVar "a") (TypeVar "a")))))), ("sometimes", Forall ["a"] (Qual [] (TypeArr (TypeArr (TypeVar "a") (TypeVar "a")) (TypeArr (TypeVar "a") (TypeVar "a"))))), ("sometimesBy", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeArr (TypeVar "a") (TypeVar "a")) (TypeArr (TypeVar "a") (TypeVar "a")))))), ("square", Forall [] (Qual [] (TypeCon "Number"))), ("tick", Forall ["a", "b"] (Qual [] (TypeArr (TypeVar "a") (TypeArr (TypeArr (TypeVar "a") (TypeVar "b")) (TypeVar "b"))))), ("time", Forall [] (Qual [] (TypeCon "Number"))), ("timeloop", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a")))))), ("true", Forall [] (Qual [] (TypeCon "Number"))), ("while", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeArr (TypeVar "a") (TypeVar "a")) (TypeArr (TypeVar "a") (TypeVar "a")))))), ("zoom", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a")))))), ("|*", Forall ["a"] (Qual [IsIn "Num" (TypeVar "a")] (TypeArr (TypeVar "a") (TypeArr (TypeVar "a") (TypeVar "a"))))), ("~>", Forall ["a"] (Qual [] (TypeArr (TypeCon "Number") (TypeArr (TypeVar "a") (TypeVar "a")))))]
