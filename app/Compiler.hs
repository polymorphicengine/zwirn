module Compiler where

import Language
import Data.List (intercalate)

compile :: Simple -> String
compile (SVar x) = x
compile (SInt i) = "(P.pure " ++ show i ++ ")"
compile (SRest) = "T.silence"
compile (SElong t) = "(" ++ compile t ++ ")"
compile (SSeq ts) = "(T.timecat " ++ "[" ++ intercalate "," ss ++  "])"
                     where ss = map (\(n,m) -> "(" ++ show n ++ "," ++ compile m ++ ")") $ resolveSize $ ts
compile (SStack ts) = "(T.stack [" ++ intercalate "," (map compile ts) ++ "])"
compile (SDiv x n) = "(T.slow " ++ compile n ++ " " ++ compile x ++ ")"
compile (SMult x n) = "(T.fast " ++ compile n ++ " " ++ compile x ++ ")"
compile (SApp x y) = "(apply " ++ compile x ++ " " ++ compile y ++ ")"
compile (SOp n x y) = "(apply (apply (" ++ n ++ ") " ++ compile x ++ ") " ++ compile y ++ ")"
compile (SLambda v x) = "(P.pure (\\" ++ v ++ " -> " ++ compile x ++"))"

resolveSize :: [Simple] -> [(Int,Simple)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: Simple -> Int
elongAmount (SElong t) = elongAmount t + 1
elongAmount _ = 1
