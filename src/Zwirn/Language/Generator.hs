module Zwirn.Language.Generator
    ( generate
    , generateDef
    , generateWithoutContext
    , generateDefWithoutContext
    ) where

import Zwirn.Language.Simple

import Data.Text (unpack)
import Data.List (intercalate)

generate :: SimpleTerm -> String
generate (SVar p x) = "(addContext " ++ generatePosition p ++ " (" ++ unpack x ++ "))"
generate (SText p x) = "(addContext " ++ generatePosition p ++ " (textPat " ++ unpack x ++ "))"
generate (SNum (Just p) x) = "(addContext " ++ generatePosition p ++ " (numPat (" ++ unpack x ++ ")))"
generate (SNum Nothing x) = "(numPat " ++ unpack x ++ ")"
generate (SRest) = "T.silence"
generate (SElong t _) = "(" ++ generate t ++ ")"
generate (SSeq ts) = "(T.timecat " ++ "[" ++ intercalate "," ss ++  "])"
                     where ss = map (\(n,m) -> "(" ++ show n ++ "," ++ generate m ++ ")") $ resolveSize $ ts
generate (SStack ts) = "(T.stack [" ++ intercalate "," (map generate ts) ++ "])"
generate (SChoice seed ts) = "(choiceBy " ++ show seed ++ " [" ++ intercalate "," (map generate ts) ++ "])"
generate (SEuclid s n m (Just k)) = "(T.euclidOff " ++ generate n ++ " " ++ generate m ++ " " ++ generate k ++ " " ++ generate s ++ ")"
generate (SEuclid s n m Nothing) = "(T.euclid " ++ generate n ++ " " ++ generate m ++ " " ++ generate s ++ ")"
generate (SApp x y) = "(apply " ++ generate x ++ " " ++ generate y ++ ")"
generate (SInfix x op y) = case unpack op of
                                   "'" -> "(apply (apply tick " ++ generate x ++ ") " ++ generate y ++ ")" -- TODO: make this a bit less hacky
                                   un -> "(apply (apply (" ++ un ++ ") " ++ generate x ++ ") " ++ generate y ++ ")"
generate (SLambda v x) = "(pat (\\" ++ unpack v ++ " -> " ++ generate x ++"))"

resolveSize :: [SimpleTerm] -> [(Int,SimpleTerm)]
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
generateWithoutContext (SText _ x) = "(textPat " ++ unpack x ++ ")"
generateWithoutContext (SNum _ x) = "(numPat " ++ unpack x ++ ")"
generateWithoutContext x = generate x

generateDefWithoutContext :: SimpleDef -> String
generateDefWithoutContext (LetS n t) = "let " ++ unpack n ++ " = " ++ generateWithoutContext t
