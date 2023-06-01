module Compiler where

import Language
import Data.List (intercalate)

compile :: Simple -> String
compile (SVar (Just p) x) = "(addContext " ++ show p ++ " (" ++ x ++ "))"
compile (SVar Nothing x) = x
compile (SRest) = "T.silence"
compile (SElong t) = "(" ++ compile t ++ ")"
compile (SSeq ts) = "(T.timecat " ++ "[" ++ intercalate "," ss ++  "])"
                     where ss = map (\(n,m) -> "(" ++ show n ++ "," ++ compile m ++ ")") $ resolveSize $ ts
compile (SStack ts) = "(pat [" ++ intercalate "," (map compile ts) ++ "])"
compile (SChoice seed ts) = "(choiceBy " ++ show seed ++ " [" ++ intercalate "," (map compile ts) ++ "])"
compile (SDiv x n) = "(T.slow " ++ compile n ++ " " ++ compile x ++ ")"
compile (SMult x n) = "(T.fast " ++ compile n ++ " " ++ compile x ++ ")"
compile (SEuclid s n m k) = "(T.euclidOff " ++ compile n ++ " " ++ compile m ++ " " ++ compile k ++ " " ++ compile s ++ ")"
compile (SApp x y) = "(apply " ++ compile x ++ " " ++ compile y ++ ")"
compile (SOp n x y) = "(apply (apply (" ++ n ++ ") " ++ compile x ++ ") " ++ compile y ++ ")"
compile (SLambda v x) = "(pat (\\" ++ v ++ " -> " ++ compile x ++"))"

resolveSize :: [Simple] -> [(Int,Simple)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: Simple -> Int
elongAmount (SElong t) = elongAmount t + 1
elongAmount _ = 1

compileDef :: SimpleDef -> String
compileDef (LetS n t) = "let " ++ n ++ " = " ++ compile t
