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

import Control.Applicative (liftA3)
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
    -- number functions
    ("|+", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("+|", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("|-", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("-|", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("|*", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("*|", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("|/", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("/|", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("range", Forall [] $ unqual $ numberT --> numberT --> numberT --> numberT),
    -- functions of time
    ("modulate", Forall ["a"] $ unqual $ (numberT --> numberT) --> varA --> varA),
    ("fast", Forall ["a"] $ unqual $ numberT --> varA --> varA),
    ("*", Forall ["a"] $ unqual $ varA --> numberT --> varA),
    ("slow", Forall ["a"] $ unqual $ numberT --> varA --> varA),
    ("/", Forall ["a"] $ unqual $ varA --> numberT --> varA),
    ("shift", Forall ["a"] $ unqual $ numberT --> varA --> varA),
    ("+", Forall ["a"] $ unqual $ varA --> numberT --> varA),
    ("-", Forall ["a"] $ unqual $ varA --> numberT --> varA),
    ("rev", Forall ["a"] $ unqual $ varA --> varA),
    ("revBy", Forall ["a"] $ unqual $ numberT --> varA --> varA),
    ("timeloop", Forall ["a"] $ unqual $ numberT --> numberT --> varA --> varA),
    -- structure
    ("euclidOff", Forall ["a"] $ unqual $ numberT --> numberT --> numberT --> varA --> varA),
    ("euclid", Forall ["a"] $ unqual $ numberT --> numberT --> varA --> varA),
    ("segment", Forall ["a"] $ unqual $ numberT --> varA --> varA),
    ("struct", Forall ["a", "b"] $ unqual $ varA --> varB --> varB),
    ("run", Forall [] $ unqual $ numberT --> numberT),
    -- ord operations
    (">=", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA --> varA),
    ("not", Forall ["a"] $ Qual [IsIn "Num" varA] $ varA --> varA),
    ("iff", Forall ["a"] $ Qual [] $ numberT --> varA --> varA --> varA),
    -- ("==", Forall ["a"] $ Qual [IsIn "Eq" varA] $ varA --> varA --> numberT),
    ("getN", Forall [] $ unqual $ textT --> numberT),
    ("getT", Forall [] $ unqual $ textT --> textT),
    ("getM", Forall [] $ unqual $ textT --> numberT),
    ("set", Forall ["a", "b"] $ unqual $ textT --> varA --> varB --> varB),
    ("modify", Forall ["a", "b", "c"] $ unqual $ textT --> (varA --> varB) --> varC --> varC),
    -- working on maps
    ("paramN", Forall [] $ unqual $ textT --> numberT --> mapT),
    ("paramT", Forall [] $ unqual $ textT --> textT --> mapT),
    ("#", Forall [] $ unqual $ mapT --> mapT --> mapT),
    ("lookupN", Forall [] $ unqual $ textT --> mapT --> numberT),
    ("lookupT", Forall [] $ unqual $ textT --> mapT --> textT)
  ]

primitives :: ExpressionMap
primitives =
  Map.fromList
    [ -- combinators
      ("id", lambda id),
      ("const", lambda $ \x -> lambda $ const x),
      ("scomb", lambda $ \f -> lambda $ \g -> lambda $ \x -> f ! x ! (g ! x)),
      -- variations of apply
      ("\'", toExp (flip squeezeApply :: Zwirn Expression -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression)),
      ("$", toExp (squeezeApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("|$", toExp (leftApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("$|", toExp (rightApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("map", toExp (mapZ :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      ("layer", toExp (layer :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)),
      -- number functions
      ("|+", toExp (liftA2Left (+) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("+|", toExp (liftA2Right (+) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("|-", toExp (liftA2Left (-) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("-|", toExp (liftA2Right (-) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("|*", toExp (liftA2Left (*) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("*|", toExp (liftA2Right (*) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("|/", toExp (liftA2Left (/) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("/|", toExp (liftA2Right (/) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("range", toExp (range :: Zwirn Double -> Zwirn Double -> Zwirn Double -> Zwirn Double)),
      -- functions of time
      ("modulate", toExp modTimeExp), -- buggy
      ("fast", toExp (fast :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)),
      ("*", toExp (flip fast :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)),
      ("slow", toExp (slow :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)),
      ("/", toExp (flip slow :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)),
      ("shift", toExp (shift :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)),
      ("+", toExp (flip shift :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)),
      ("-", toExp (flip (shift . fmap negate) :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)),
      ("rev", toExp (rev :: Zwirn Expression -> Zwirn Expression)),
      ("revBy", toExp (revBy :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)),
      -- buggy
      ("timeloop", toExp (timeloop :: Zwirn Time -> Zwirn Time -> Zwirn Expression -> Zwirn Expression)),
      -- structure
      ("euclidOff", toExp (euclidOff :: Zwirn Int -> Zwirn Int -> Zwirn Int -> Zwirn Expression -> Zwirn Expression)),
      ("euclid", toExp (euclid :: Zwirn Int -> Zwirn Int -> Zwirn Expression -> Zwirn Expression)),
      ("segment", toExp (segment :: Zwirn Int -> Zwirn Expression -> Zwirn Expression)),
      ("struct", toExp (struct :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("run", toExp (run :: Zwirn Int -> Zwirn Int)),
      -- ord operations
      (">=", toExp (liftA2 (pervasive2 @Double (\d e -> if d >= e then 1 else 0)) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      -- boolean opeartions
      ("not", toExp (fmap $ pervasive not :: Zwirn Expression -> Zwirn Expression)),
      ("iff", toExp iff),
      ("&&", toExp (liftA2 (pervasive2 (&&)) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      ("||", toExp (liftA2 (pervasive2 (||)) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)),
      -- operations on state
      ("getN", toExp getStateN),
      ("getT", toExp getStateT),
      ("getM", toExp getStateM),
      ("set", toExp setState),
      ("modify", toExp modifyStateN),
      -- working on maps
      ("paramN", toExp singMap),
      ("paramT", toExp singMap),
      ("#", toExp (liftA2 Map.union :: Zwirn ExpressionMap -> Zwirn ExpressionMap -> Zwirn ExpressionMap)),
      ("lookupN", toExp lookN),
      ("lookupT", toExp lookT)
    ]
