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
compile (TLambda ps) = "(P.pure (\\pat -> " ++ compileCases ps ++"))"


resolveSize :: [Term] -> [(Int,Term)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: Term -> Int
elongAmount (TElong t) = elongAmount t + 1
elongAmount _ = 1

compilePat :: Pat -> String
compilePat (PVar x) = x
compilePat (PInt i) = "(FVal " ++ show i ++ ")"
compilePat (PBool b) = "(FVal P." ++ show b ++ ")"
compilePat PEmpty = "FEmpty"
compilePat (PSeq p1 p2) = "(FSeq " ++ compilePat p1 ++ " " ++ compilePat p2 ++ ")"
compilePat (PStack p1 p2) = "(FStack " ++ compilePat p1 ++ " " ++ compilePat p2 ++ ")"
compilePat (PDiv p1 p2) = "(FDiv " ++ compilePat p1 ++ " " ++ compilePat p2 ++ ")"
compilePat (PMult p1 p2) = "(FMult " ++ compilePat p1 ++ " " ++ compilePat p2 ++ ")"

compileCase :: (Pat,Term) -> String
compileCase (p,t) = compilePat p ++ " -> " ++ compile t

compileCases :: [(Pat,Term)] -> String
compileCases ps = "(case pat of " ++ (intercalate ";" $ map compileCase ps) ++ ")"
