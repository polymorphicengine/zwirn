module Zwirn.Language.Compiler where

import Zwirn.Language.Syntax
import Data.List (intercalate)
import Data.Text (unpack)

compile :: SimpleTerm -> String
compile (SVar (Just p) x) = "(addContext " ++ compilePosition p ++ " (" ++ unpack x ++ "))"
compile (SVar Nothing x) = unpack x
compile (SRest) = "T.silence"
compile (SElong t) = "(" ++ compile t ++ ")"
compile (SSeq ts) = "(T.timecat " ++ "[" ++ intercalate "," ss ++  "])"
                     where ss = map (\(n,m) -> "(" ++ show n ++ "," ++ compile m ++ ")") $ resolveSize $ ts
compile (SStack ts) = "(pat [" ++ intercalate "," (map compile ts) ++ "])"
compile (SChoice seed ts) = "(choiceBy " ++ show seed ++ " [" ++ intercalate "," (map compile ts) ++ "])"
compile (SEuclid s n m k) = "(T.euclidOff " ++ compile n ++ " " ++ compile m ++ " " ++ compile k ++ " " ++ compile s ++ ")"
compile (SApp x y) = "(apply " ++ compile x ++ " " ++ compile y ++ ")"
compile (SOp x op y) = "(apply (apply (" ++ unpack op ++ ") " ++ compile x ++ ") " ++ compile y ++ ")"
compile (SLambda v x) = "(pat (\\" ++ unpack v ++ " -> " ++ compile x ++"))"

resolveSize :: [SimpleTerm] -> [(Int,SimpleTerm)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: SimpleTerm -> Int
elongAmount (SElong t) = elongAmount t + 1
elongAmount _ = 1

compileDef :: SimpleDef -> String
compileDef (LetS n t) = "let " ++ unpack n ++ " = " ++ compile t

compilePosition :: Position -> String
compilePosition (Pos l s e editor) = "((" ++ show l ++ "," ++ show s ++ "),(" ++ show editor ++ "," ++ show e ++ "))"

-- without context

compileWithoutContext :: SimpleTerm -> String
compileWithoutContext (SVar _ x) = unpack x
compileWithoutContext x = compile x

compileDefWithoutContext :: SimpleDef -> String
compileDefWithoutContext (LetS n t) = "let " ++ unpack n ++ " = " ++ compileWithoutContext t