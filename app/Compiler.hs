module Compiler where

import Language
import Data.List (intercalate)

compile :: Term -> String
compile (TVar x) = x
compile (TInt i) = "(P.pure " ++ show i ++ ")"
compile (TBool b) = "(P.pure P." ++ show b ++ ")"
compile (TEmpty) = "T.silence"
compile (TRest) = "T.silence"
compile (TElong t) = "(" ++ compile t ++ ")"
compile t@(TSeq _ _) = "(T.timecat " ++ "[" ++ intercalate "," ts ++  "])"
                     where ts = map (\(n,m) -> "(" ++ show n ++ "," ++ compile m ++ ")") $ resolveSize $ getTSeq t
compile (TStack x y) = "(T.stack [" ++ compile x ++ "," ++ compile y ++ "])"
compile (TDiv x n) = "(T.slow " ++ compile n ++ " " ++ compile x ++ ")"
compile (TMult x n) = "(T.fast " ++ compile n ++ " " ++ compile x ++ ")"
compile (TApp x y) = "(apply " ++ compile x ++ " " ++ compile y ++ ")"
compile (TLambda v x) = "(P.pure (\\" ++ v ++ " -> " ++ compile x ++"))"

resolveSize :: [Term] -> [(Int,Term)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: Term -> Int
elongAmount (TElong t) = elongAmount t + 1
elongAmount _ = 1
