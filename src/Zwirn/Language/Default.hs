{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Zwirn.Language.Default
  ( defaultTypeEnv,
    primitives,
  )
where

{-
    Default.hs - provides the default type environment and expression map
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
import Data.Text (Text)
import Sound.Zwirn.Core.Cord
import Sound.Zwirn.Core.Core
import Sound.Zwirn.Core.Types hiding (Zwirn)
import Sound.Zwirn.Time
import Zwirn.Language.Evaluate
import Zwirn.Language.TypeCheck.Env
import Zwirn.Language.TypeCheck.Types

defaultTypeEnv :: TypeEnv
defaultTypeEnv = TypeEnv (Map.fromList primitiveTypes) defaultInstances

defaultInstances :: [Instance]
defaultInstances =
  [ IsIn "Num" numberT,
    IsIn "Num" mapT,
    IsIn "Eq" numberT,
    IsIn "Eq" mapT,
    IsIn "Eq" textT
  ]

primitiveTypes :: [(Text, Scheme)]
primitiveTypes =
  [ ("id", Forall ["a"] $ unqual $ varA --> varA),
    ("const", Forall ["a", "b"] $ unqual $ varA --> varB --> varA),
    ("scomb", Forall ["a", "b"] $ unqual $ (varA --> varB --> varC) --> (varA --> varB) --> varA --> varC),
    ("\'", Forall ["a", "b"] $ unqual $ varA --> (varA --> varB) --> varB),
    ("$", Forall ["a", "b"] $ unqual $ (varA --> varB) --> varA --> varB),
    ("|$", Forall ["a", "b"] $ unqual $ (varA --> varB) --> varA --> varB),
    ("$|", Forall ["a", "b"] $ unqual $ (varA --> varB) --> varA --> varB),
    ("map", Forall ["a", "b"] $ unqual $ (varA --> varB) --> varA --> varB),
    ("layer", Forall ["a", "b"] $ unqual $ (varA --> varB) --> varA --> varB),
    ("|+", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("+|", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("|*", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("*|", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    (">=", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("not", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA),
    ("==", Forall ["a"] $ Qual [IsIn "Eq" varA] $ varA --> varA --> numberT),
    ("paramN", Forall [] $ unqual $ textT --> numberT --> mapT),
    ("paramT", Forall [] $ unqual $ textT --> textT --> mapT),
    ("#", Forall [] $ unqual $ mapT --> mapT --> mapT),
    ("getN", Forall [] $ unqual $ textT --> numberT),
    ("getT", Forall [] $ unqual $ textT --> textT),
    ("getM", Forall [] $ unqual $ textT --> numberT),
    ("lookupN", Forall [] $ unqual $ textT --> mapT --> numberT),
    ("lookupT", Forall [] $ unqual $ textT --> mapT --> textT)
  ]

primitives :: ExpressionMap
primitives =
  Map.fromList
    [ ("id", lambda id),
      ("const", lambda $ \x -> lambda $ const x),
      ("scomb", lambda $ \f -> lambda $ \g -> lambda $ \x -> f ! x ! (g ! x)),
      ("\'", toExp (flip squeezeApply :: Zwirn Expression -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression)),
      ("$", toExp (squeezeApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("|$", toExp (leftApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("$|", toExp (rightApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("map", toExp (cordMap :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("layer", toExp (layer :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("|+", toExp (liftA2Left (+) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("+|", toExp (liftA2Right (+) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("|*", toExp (liftA2Left (*) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("*|", toExp (liftA2Right (*) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("fast", toExp (lift fast :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)),
      ("rev", toExp (rev :: Zwirn Expression -> Zwirn Expression)),
      ("paramN", toExp singMap),
      ("paramT", toExp singMap),
      ("#", toExp (liftA2 Map.union :: Zwirn ExpressionMap -> Zwirn ExpressionMap -> Zwirn ExpressionMap)),
      (">=", toExp (liftA2 (pervasive2 @Double (\d e -> if d >= e then 1 else 0)) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("not", toExp (fmap $ pervasive not :: Zwirn Expression -> Zwirn Expression)),
      ("&&", toExp (liftA2 (pervasive2 (&&)) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("||", toExp (liftA2 (pervasive2 (||)) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("getN", toExp getStateN),
      ("getT", toExp getStateT),
      ("getM", toExp getStateM),
      ("lookupN", toExp lookN),
      ("lookupT", toExp lookT)
    ]
