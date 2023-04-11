module Compiler where

import Language
import Data.List (intercalate)

compile :: Term -> String
compile (TVar x) = x
compile (TInt i) = "(FVal " ++ show i ++ ")"
compile (TBool b) = "(FVal P." ++ show b ++ ")"
compile (TEmpty) = "FEmpty"
compile (TRest) = "FRest"
compile (TElong t) = "(FElong " ++ compile t ++ ")"
compile (TSeq x y) = "(FSeq " ++ compile x ++ " " ++ compile y ++ ")"
compile (TStack x y) = "(FStack " ++ compile x ++ " " ++ compile y ++ ")"
compile (TDiv x n) = "(FDiv " ++ compile x ++ " " ++ compile n ++ ")"
compile (TMult x n) = "(FMult " ++ compile x ++ " " ++ compile n ++ ")"
compile (TApp x y) = "(apply (toPattern " ++ compile x ++ ") (toPattern " ++ compile y ++ "))"
compile (TLambda ps) = "(FVal (\\pat -> " ++ compileCases ps ++"))"

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
