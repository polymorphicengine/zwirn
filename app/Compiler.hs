module Compiler where

import Language
import Data.List (intercalate)

compile :: Term -> String
compile (TVar x) = x
compile (TInt i) = "(P.pure " ++ show i ++ ")"
compile (TRest) = "T.silence"
compile (TElong t) = "(" ++ compile t ++ ")"
compile (TSeq ts) = "(T.timecat " ++ "[" ++ intercalate "," ss ++  "])"
                     where ss = map (\(n,m) -> "(" ++ show n ++ "," ++ compile m ++ ")") $ resolveSize $ ts
compile (TStack ts) = "(T.stack [" ++ intercalate "," (map compile ts) ++ "])"
compile (TDiv x n) = "(T.slow " ++ compile n ++ " " ++ compile x ++ ")"
compile (TMult x n) = "(T.fast " ++ compile n ++ " " ++ compile x ++ ")"
compile (TApp x y) = "(apply " ++ compile x ++ " " ++ compile y ++ ")"
compile (TLambda v x) = "(P.pure (\\" ++ v ++ " -> " ++ compile x ++"))"

resolveSize :: [Term] -> [(Int,Term)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: Term -> Int
elongAmount (TElong t) = elongAmount t + 1
elongAmount _ = 1
