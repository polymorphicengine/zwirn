module Compiler where

import Language
import Data.List (intercalate)

compile :: Term -> String
compile (TVar x) = "(P.pure " ++ x ++ ")"
compile (TInt i) = "(P.pure " ++ show i ++ ")"
compile (TBool b) = "(P.pure P." ++ show b ++ ")"
compile (TEmpty) = "silence"
compile (TRest) = "silence"
compile (TElong t) = "(" ++ compile t ++ ")"
compile t@(TSeq _ _) = "(timecat " ++ "[" ++ intercalate "," ts ++  "])"
                     where ts = map (\(n,m) -> "(" ++ show n ++ "," ++ compile m ++ ")") $ resolveSize $ getTSeq t
compile (TStack x y) = "(stack [" ++ compile x ++ "," ++ compile y ++ "])"
compile (TDiv x n) = "(slow " ++ compile n ++ " " ++ compile x ++ ")"
compile (TMult x n) = "(fast " ++ compile n ++ " " ++ compile x ++ ")"
compile (TApp x y) = "(apply " ++ compile x ++ " " ++ compile y ++ ")"
compile (TLambda v x) = "(P.pure (\\" ++ v ++ " -> " ++ compile' [v] x ++"))"

compile' :: [Var] -> Term -> String
compile' v (TVar x) = case elem x v of
                          True -> x
                          False -> "(P.pure " ++ x ++ ")"
compile' _ (TInt i) = "(P.pure " ++ show i ++ ")"
compile' _ (TBool b) = "(P.pure P." ++ show b ++ ")"
compile' _ (TEmpty) = "silence"
compile' _ (TRest) = "silence"
compile' v (TElong t) = "(" ++ compile' v t ++ ")"
compile' v t@(TSeq _ _) = "(timecat " ++ "[" ++ intercalate "," ts ++  "])"
                     where ts = map (\(n,m) -> "(" ++ show n ++ "," ++ compile' v m ++ ")") $ resolveSize $ getTSeq t
compile' v (TStack x y) = "(stack [" ++ compile' v x ++ "," ++ compile' v y ++ "])"
compile' v (TDiv x n) = "(slow " ++ compile' v n ++ " " ++ compile' v x ++ ")"
compile' v (TMult x n) = "(fast " ++ compile' v n ++ " " ++ compile' v x ++ ")"
compile' v (TApp x y) = "(apply " ++ compile' v x ++ " " ++ compile' v y ++ ")"
compile' v (TLambda w x) = "(P.pure (\\" ++ w ++ " -> " ++ compile' (w:v) x ++"))"

resolveSize :: [Term] -> [(Int,Term)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: Term -> Int
elongAmount (TElong t) = elongAmount t + 1
elongAmount _ = 1
