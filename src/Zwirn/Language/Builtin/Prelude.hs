{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Zwirn.Language.Builtin.Prelude
  ( builtinEnvironment,
  )
where

{-
    Builtin.hs - defines builtin functions
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
import Zwirn.Core.Conditional as Z
import Zwirn.Core.Cord
import Zwirn.Core.Core
import Zwirn.Core.Modulate
import Zwirn.Core.Number
import Zwirn.Core.Structure
import Zwirn.Core.Time
import Zwirn.Language.Builtin.Internal
import Zwirn.Language.Builtin.Parameters
import Zwirn.Language.Environment
import Zwirn.Language.Evaluate hiding (insert)
import Zwirn.Language.TypeCheck.Types

builtinEnvironment :: InterpreterEnv
builtinEnvironment = IEnv builtins instances

instances :: [Instance]
instances =
  [ IsIn "Num" numberT,
    IsIn "Num" mapT,
    IsIn "Eq" numberT,
    IsIn "Eq" mapT,
    IsIn "Eq" textT
  ]

builtins :: Map.Map Text AnnotatedExpression
builtins =
  Map.unions $
    [ "id"
        === lambda id
        <:: "a -> a"
        --| "identity function",
      "const"
        === lambda (lambda . const)
        <:: "a -> b -> a"
        --| "constant function - ignore second input",
      "scomb"
        === lambda (\f -> lambda $ \g -> lambda $ \x -> f ! x ! (g ! x))
        <:: "(a -> b -> c) -> (a -> b) -> a -> c"
        --| "S-combinator",
      "\'"
        === toExp (flip squeezeApply :: Zwirn Expression -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression)
        <:: "a -> (a -> b) -> b"
        --| "apply argument to function, results are squeezed",
      "$"
        === toExp (squeezeApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)
        <:: "(a -> b) -> a -> b"
        --| "apply argument to function, results are squeezed",
      "|$"
        === toExp (outerApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)
        <:: "(a -> b) -> a -> b"
        --| "apply argument to function",
      "$|"
        === toExp (innerApply :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)
        <:: "(a -> b) -> a -> b"
        --| "apply argument to function",
      "map"
        === toExp (mapZ :: Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)
        <:: "(a -> b) -> a -> b"
        --| "map a function over the structure of the argument",
      "|+"
        === toExp ((+) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a -> a"
        --| "addition",
      "|-"
        === toExp ((-) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a -> a"
        --| "subtraction",
      "|*"
        === toExp ((*) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a -> a"
        --| "multiplication",
      "|/"
        === toExp ((/) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a -> a"
        --| "division",
      "range"
        === toExp (range :: Zwirn Double -> Zwirn Double -> Zwirn Double -> Zwirn Double)
        <:: "Number -> Number -> Number -> Number"
        --| "range x y l maps number l linearly into interval (x,y), assuming l is between 0 and 1",
      "*"
        === toExp (flip fast :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)
        <:: "a -> Number -> a"
        --| "multiply time, making it faster",
      "fast"
        === toExp (fast :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "multiply time, making it faster",
      "/"
        === toExp (flip slow :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)
        <:: "a -> Number -> a"
        --| "divide time, making it slower",
      "slow"
        === toExp (slow :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "divide time, making it slower",
      "+"
        === toExp (flip shift :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)
        <:: "a -> Number -> a"
        --| "shift time to the right",
      "-"
        === toExp (flip (shift . fmap negate) :: Zwirn Expression -> Zwirn Time -> Zwirn Expression)
        <:: "a -> Number -> a"
        --| "shift time to the left",
      "shift"
        === toExp (shift :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "shift time",
      "revBy"
        === toExp (revBy :: Zwirn Time -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "reverse time, piecewise",
      "rev"
        === toExp (rev :: Zwirn Expression -> Zwirn Expression)
        <:: "a -> a"
        --| "reverse time completely",
      "euclidOff"
        === toExp (euclidOff :: Zwirn Int -> Zwirn Int -> Zwirn Int -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> Number -> Number -> a -> a"
        --| "shifted euclidean rhythm",
      "euclid"
        === toExp (euclid :: Zwirn Int -> Zwirn Int -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> Number -> a -> a"
        --| "euclidean rhythm",
      "segment"
        === toExp (segment :: Zwirn Int -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "divide structure into equal pieces",
      "struct"
        === toExp (struct :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "a -> b -> b"
        --| "copy the structure from first value",
      "run"
        === toExp (run :: Zwirn Int -> Zwirn Int)
        <:: "Number -> Number"
        --| "run n == [0 .. n-1]",
      "=="
        === toExp (eq :: Zwirn Expression -> Zwirn Expression -> Zwirn Bool)
        <:: "Eq a => a -> a -> Number"
        --| "equality",
      ">="
        === toExp (geq :: Zwirn Expression -> Zwirn Expression -> Zwirn Bool)
        <:: "Number -> Number -> Number"
        --| "greater or equal",
      "<="
        === toExp (leq :: Zwirn Expression -> Zwirn Expression -> Zwirn Bool)
        <:: "Number -> Number -> Number"
        --| "less or equal",
      "<"
        === toExp (ge :: Zwirn Expression -> Zwirn Expression -> Zwirn Bool)
        <:: "Number -> Number -> Number"
        --| "less",
      ">"
        === toExp (le :: Zwirn Expression -> Zwirn Expression -> Zwirn Bool)
        <:: "Number -> Number -> Number"
        --| "greater",
      "not"
        === toExp (Z.not :: Zwirn Bool -> Zwirn Bool)
        <:: "Number -> Number"
        --| "equality",
      "&&"
        === toExp (Z.and :: Zwirn Bool -> Zwirn Bool -> Zwirn Bool)
        <:: "Number -> Number -> Number"
        --| "greater or equal",
      "||"
        === toExp (Z.or :: Zwirn Bool -> Zwirn Bool -> Zwirn Bool)
        <:: "Number -> Number -> Number"
        --| "less or equal",
      "ifthen"
        === toExp (ifthen :: Zwirn Bool -> Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a -> a"
        --| "less",
      "if"
        === toExp (iff :: Zwirn Bool -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "greater",
      "project"
        === toExp (project :: Zwirn Int -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "project to a certain layer of a cord",
      "insert"
        === toExp (insert :: Zwirn Int -> Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a -> a"
        --| "insert into a specific layer of a cord",
      "remove"
        === toExp (remove :: Zwirn Int -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "remove a specific layer of a cord",
      "at"
        === toExp (at :: Zwirn Int -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> (a -> a) -> a -> a"
        --| "apply a function to a specific layer of a cord",
      "pN"
        === toExp (singMap :: Zwirn Text -> Zwirn Double -> Zwirn Expression)
        <:: "Text -> Number -> Map"
        --| "number singleton with specific key",
      "pT"
        === toExp (singMap :: Zwirn Text -> Zwirn Text -> Zwirn Expression)
        <:: "Text -> Text -> Map"
        --| "text singleton with specific key",
      "#"
        === toExp (liftA2 Map.union :: Zwirn ExpressionMap -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Map -> Map -> Map"
        --| "union of two maps - structure from the left",
      "lookupN"
        === toExp lookN
        <:: "Text -> Map -> Number"
        --| "retrieve number at given key or silence if key is missing or it's value not a number",
      "lookupT"
        === toExp lookT
        <:: "Text -> Map -> Text"
        --| "retrieve text at given key or silence if key is missing or it's value not a text",
      "getN"
        === toExp getStateN
        <:: "Text -> Number"
        --| "retrieve number from state at given key or silence if key is missing or it's value not a number",
      "getT"
        === toExp getStateT
        <:: "Text -> Text"
        --| "retrieve text from state at given key or silence if key is missing or it's value not a text",
      "getM"
        === toExp getStateM
        <:: "Text -> Map"
        --| "retrieve map from state at given key or silence if key is missing or it's value not a map",
      "set"
        === toExp setState
        <:: "Text -> a -> b -> b"
        --| "set state at key to given value",
      "modify"
        === toExp modifyState
        <:: "Text -> (a -> a) -> b -> b"
        --| "modify state at given key with function"
    ]
      ++ builtinParams
