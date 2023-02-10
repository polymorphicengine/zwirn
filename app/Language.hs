module Language where

import Data.List (intercalate)


type Var = String

data Pat = PVar Var
         | PInt Int
         | PBool Bool
         | PEmpty
         | PSeq Pat Pat
         | PStack Pat Pat
         | PDiv Pat Pat
         | PMult Pat Pat
         deriving (Eq, Show)

data Term = TVar Var
          | TInt Int
          | TBool Bool
          | TRest
          | TEmpty
          | TElong Term
          | TSeq Term Term
          | TStack Term Term
          | TMult Term Term
          | TDiv Term Term
          | TLambda [(Pat,Term)]
          | TApp Term Term
          deriving (Eq, Show)

displayPat :: Pat -> String
displayPat (PVar x) = x
displayPat (PInt i) = show i
displayPat (PBool b) = show b
displayPat (PSeq x y) = "(" ++ displayPat x ++ " " ++ displayPat y ++ ")"
displayPat (PStack x y) = "(" ++ displayPat x ++ "," ++ displayPat y ++ ")"
displayPat (PEmpty) = "nil"
displayPat (PDiv x n) = displayPat x ++ "/" ++ displayPat n
displayPat (PMult x n) = displayPat x ++ "*" ++ displayPat n

displayTerm :: Term -> String
displayTerm (TVar x) = x
displayTerm (TInt i) = show i
displayTerm (TBool True) = "t"
displayTerm (TBool False) = "f"
displayTerm (TRest) = "~"
displayTerm TEmpty = ""
displayTerm (TElong t) = displayTerm t  ++ "@"
displayTerm t@(TSeq _ _) = "(" ++ (intercalate " " $ map displayTerm (getTSeq t)) ++ ")"
displayTerm (TStack t1 t2) = displayTerm t1 ++ "," ++ displayTerm t2
displayTerm (TMult t1 t2) = displayTerm t1 ++ "*" ++ displayTerm t2
displayTerm (TDiv t1 t2) = displayTerm t1 ++ "/" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TLambda ps) = "(\\" ++ (intercalate "|" $ map (\(p,t) -> displayPat p ++ "." ++ displayTerm t) ps) ++ ")"


getTSeq :: Term -> [Term]
getTSeq (TSeq t1 t2) = t1:(getTSeq t2)
getTSeq t = [t]

toTSeq :: [Term] -> Term
toTSeq [] = TRest
toTSeq [t] = TSeq t TEmpty
toTSeq (t:ts) = TSeq t (toTSeq ts)
