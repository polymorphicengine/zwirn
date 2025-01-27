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
import Zwirn.Core.Cord as C
import Zwirn.Core.Core
import Zwirn.Core.Map as M
import Zwirn.Core.Modulate
import Zwirn.Core.Number as N
import Zwirn.Core.Random
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
      "."
        === lambda (\g -> lambda $ \f -> lambda $ \x -> g ! (f ! x))
        <:: "(b -> c) -> (a -> b) -> a -> c"
        --| "function composition",
      "flip"
        === lambda (\f -> lambda $ \y -> lambda $ \x -> f ! x ! y)
        <:: "(a -> b -> c) -> b -> a -> c"
        --| "flip arguments",
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
      "negate"
        === toExp (negate :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "negate",
      "abs"
        === toExp (abs :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "absolute value",
      "signum"
        === toExp (signum :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a =>  a -> a"
        --| "signum",
      "recip"
        === toExp (recip :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "reciprocal value",
      "pi"
        === toExp (pi :: Zwirn Expression)
        <:: "Number"
        --| "pi",
      "|**"
        === toExp ((**) :: Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a -> a"
        --| "exponentiation",
      "exp"
        === toExp (exp :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "exponential function",
      "log"
        === toExp (log :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "logarithm base 10",
      "sqrt"
        === toExp (sqrt :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a =>  a -> a"
        --| "square root",
      "sin"
        === toExp (sin :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "sine function",
      "cos"
        === toExp (cos :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "cosine function",
      "tan"
        === toExp (tan :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "tangens",
      "asin"
        === toExp (asin :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "arc sine function",
      "acos"
        === toExp (acos :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "arc cosine function",
      "atan"
        === toExp (atan :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "arc tangens",
      "sinh"
        === toExp (sinh :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "hyperbolic sine",
      "cosh"
        === toExp (cosh :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a =>  a -> a"
        --| "hyperbolic cosine",
      "tanh"
        === toExp (tan :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "hyperbolic tangens",
      "asinh"
        === toExp (asinh :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "hyperbolic arc sine function",
      "acosh"
        === toExp (acosh :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "hyperbolic arc cosine function",
      "atanh"
        === toExp (atanh :: Zwirn Expression -> Zwirn Expression)
        <:: "Num a => a -> a"
        --| "hyperbolic arc tangens",
      "mod"
        === toExp (N.mod :: Zwirn Double -> Zwirn Double -> Zwirn Double)
        <:: "Number -> Number -> Number"
        --| "modulo",
      "frac"
        === toExp (N.frac :: Zwirn Double -> Zwirn Double)
        <:: "Number -> Number"
        --| "fractional part of a number",
      "trunc"
        === toExp (N.trunc :: Zwirn Double -> Zwirn Int)
        <:: "Number -> Number"
        --| "truncate",
      "ceil"
        === toExp (N.ceil :: Zwirn Double -> Zwirn Int)
        <:: "Number -> Number"
        --| "round up",
      "floor"
        === toExp (N.floor :: Zwirn Double -> Zwirn Int)
        <:: "Number -> Number"
        --| "round down",
      "round"
        === toExp (N.round :: Zwirn Double -> Zwirn Int)
        <:: "Number -> Number"
        --| "round to closest",
      "gcd"
        === toExp (N.gcd :: Zwirn Int -> Zwirn Int -> Zwirn Int)
        <:: "Number -> Number -> Number"
        --| "greatest common divisor",
      "lcm"
        === toExp (N.lcm :: Zwirn Int -> Zwirn Int -> Zwirn Int)
        <:: "Number -> Number -> Number"
        --| "least common multiple",
      "range"
        === toExp (range :: Zwirn Double -> Zwirn Double -> Zwirn Double -> Zwirn Double)
        <:: "Number -> Number -> Number -> Number"
        --| "range x y l maps number l linearly into interval (x,y), assuming l is between 0 and 1",
      "sine"
        === toExp (sine :: Zwirn Time)
        <:: "Number"
        --| "sine signal",
      "sine2"
        === toExp (sine2 :: Zwirn Time)
        <:: "Number"
        --| "bipolar sine signal",
      "saw"
        === toExp (saw :: Zwirn Time)
        <:: "Number"
        --| "saw signal",
      "saw2"
        === toExp (saw2 :: Zwirn Time)
        <:: "Number"
        --| "bipolar saw signal",
      "cosine"
        === toExp (cosine :: Zwirn Time)
        <:: "Number"
        --| "cosine signal",
      "cosine2"
        === toExp (cosine2 :: Zwirn Time)
        <:: "Number"
        --| "bipolar cosine signal",
      "isaw"
        === toExp (isaw :: Zwirn Time)
        <:: "Number"
        --| "inverse saw signal",
      "isaw2"
        === toExp (isaw2 :: Zwirn Time)
        <:: "Number"
        --| "bipolar inverse saw signal",
      "tri"
        === toExp (tri :: Zwirn Time)
        <:: "Number"
        --| "triangle signal",
      "tri2"
        === toExp (tri2 :: Zwirn Time)
        <:: "Number"
        --| "bipolar triangle signal",
      "square"
        === toExp (square :: Zwirn Time)
        <:: "Number"
        --| "square signal",
      "square2"
        === toExp (square2 :: Zwirn Time)
        <:: "Number"
        --| "bipolar square signal",
      "noise"
        === toExp (noise :: Zwirn Double)
        <:: "Number"
        --| "random stream of values between 0 and 1",
      "irand"
        === toExp (irand :: Zwirn Int -> Zwirn Int)
        <:: "Number -> Number"
        --| "random integer values between 0 and given input",
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
        === toExp (C.insert :: Zwirn Int -> Zwirn Expression -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a -> a"
        --| "insert into a specific layer of a cord",
      "remove"
        === toExp (remove :: Zwirn Int -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> a -> a"
        --| "remove a specific layer of a cord",
      "arp"
        === toExp (arp :: Zwirn Expression -> Zwirn Expression)
        <:: "a -> a"
        --| "arpeggiate",
      "reverse"
        === toExp (reverseC :: Zwirn Expression -> Zwirn Expression)
        <:: "a -> a"
        --| "reverse order of cord",
      "invert"
        === toExp (invertC :: Zwirn Expression -> Zwirn Expression)
        <:: "Number -> Number"
        --| "chord inversion",
      "rotate"
        === toExp (rotateC :: Zwirn Expression -> Zwirn Expression)
        <:: "a -> a"
        --| "cord rotation",
      "at"
        === toExp (at :: Zwirn Int -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression)
        <:: "Number -> (a -> a) -> a -> a"
        --| "apply a function to a specific layer of a cord",
      "pN"
        === toExp ((\t -> fmap toExp . singleton t) :: Zwirn Text -> Zwirn Double -> Zwirn Expression)
        <:: "Text -> Number -> Map"
        --| "number singleton with specific key",
      "pT"
        === toExp ((\t -> fmap toExp . singleton t) :: Zwirn Text -> Zwirn Text -> Zwirn Expression)
        <:: "Text -> Text -> Map"
        --| "text singleton with specific key",
      "#"
        === toExp (union :: Zwirn ExpressionMap -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Map -> Map -> Map"
        --| "union of two maps - structure from the left",
      "lookupN"
        === toExp (M.lookup :: Zwirn Text -> Zwirn ExpressionMap -> Zwirn Expression)
        <:: "Text -> Map -> Number"
        --| "retrieve number at given key or silence if key is missing or it's value not a number",
      "lookupT"
        === toExp (M.lookup :: Zwirn Text -> Zwirn ExpressionMap -> Zwirn Expression)
        <:: "Text -> Map -> Text"
        --| "retrieve text at given key or silence if key is missing or it's value not a text",
      "fix"
        === toExp (M.fix :: Zwirn Text -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Text -> (Map -> Map) -> Map -> Map"
        --| "apply a function to a specific key",
      "loopAt"
        === toExp (loopAt :: Zwirn Time -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Number -> Map -> Map"
        --| "",
      "slice"
        === toExp (slice :: Zwirn Int -> Zwirn Int -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Number -> Number -> Map -> Map"
        --| "slice a sample into equal btis and index into them",
      "chop"
        === toExp (chop :: Zwirn Int -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Number -> Map -> Map"
        --| "",
      "striate"
        === toExp (striate :: Zwirn Int -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Number -> Map -> Map"
        --| "",
      "striateBy"
        === toExp (striateBy :: Zwirn Int -> Zwirn Expression -> Zwirn ExpressionMap -> Zwirn ExpressionMap)
        <:: "Number -> Number -> Map -> Map"
        --| "",
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
        --| "modify state at given key with function",
      builtinParams
    ]
