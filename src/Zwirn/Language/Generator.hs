module Zwirn.Language.Generator
  ( generate,
    generateDef,
    generateWithoutContext,
    generateDefWithoutContext,
  )
where

{-
    Generator.hs - generate haskell source code from the
    simplified zwirn AST
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

import Data.List (intercalate)
import Data.Text (unpack)
import Zwirn.Language.Simple

generate :: SimpleTerm -> String
generate (SVar p x) = "(" ++ unpack x ++ ")"
generate (SText p x) = "(_textPat " ++ unpack x ++ ")"
generate (SNum _ x) = "(_numPat " ++ unpack x ++ ")"
-- generate (SNum Nothing x) = "(_numPat " ++ unpack x ++ ")"
generate (SRest) = "silence"
generate (SElong t _) = "(" ++ generate t ++ ")"
generate (SSeq ts) = "(Z.timecat " ++ "[" ++ intercalate "," ss ++ "])"
  where
    ss = map (\(n, m) -> "(" ++ show n ++ "," ++ generate m ++ ")") $ resolveSize $ ts
generate (SStack ts) = "(Z.stack [" ++ intercalate "," (map generate ts) ++ "])"
generate (SChoice seed ts) = "(_choiceBy " ++ show seed ++ " [" ++ intercalate "," (map generate ts) ++ "])"
generate (SEuclid s n m (Just k)) = "(_euclidOff " ++ generate n ++ " " ++ generate m ++ " " ++ generate k ++ " " ++ generate s ++ ")"
generate (SEuclid s n m Nothing) = "(_euclid " ++ generate n ++ " " ++ generate m ++ " " ++ generate s ++ ")"
generate (SApp x y) = "(_apply " ++ generate x ++ " " ++ generate y ++ ")"
generate (SInfix x op y) = case unpack op of
  "'" -> "(_apply (_apply tick " ++ generate x ++ ") " ++ generate y ++ ")" -- TODO: make this a bit less hacky
  un -> "(_apply (_apply (" ++ un ++ ") " ++ generate x ++ ") " ++ generate y ++ ")"
generate (SLambda v x) = "(_pat (\\" ++ unpack v ++ " -> " ++ generate x ++ "))"
generate (SBracket x) = "(" ++ generate x ++ ")"

resolveSize :: [SimpleTerm] -> [(Int, SimpleTerm)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: SimpleTerm -> Int
elongAmount (SElong _ i) = i
elongAmount _ = 1

generateDef :: SimpleDef -> String
generateDef (LetS n t) = "let " ++ unpack n ++ " = " ++ generate t

generatePosition :: Position -> String
generatePosition (Pos l s e editor) = "((" ++ show l ++ "," ++ show s ++ "),(" ++ show editor ++ "," ++ show e ++ "))"

-- without context

generateWithoutContext :: SimpleTerm -> String
generateWithoutContext (SVar _ x) = unpack x
generateWithoutContext (SText _ x) = "(_textPat " ++ unpack x ++ ")"
generateWithoutContext (SNum _ x) = "(_numPat " ++ unpack x ++ ")"
generateWithoutContext x = generate x

generateDefWithoutContext :: SimpleDef -> String
generateDefWithoutContext (LetS n t) = "let " ++ unpack n ++ " = " ++ generateWithoutContext t
