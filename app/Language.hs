module Language where

import Data.List (intercalate)


type Var = String

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
          | TLambda Var Term
          | TApp Term Term
          deriving (Eq, Show)

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
displayTerm (TLambda v t) = "(\\" ++ v ++ " -> " ++ displayTerm t ++ ")"


getTSeq :: Term -> [Term]
getTSeq TEmpty = []
getTSeq (TSeq t1 t2) = t1:(getTSeq t2)
getTSeq x = [x]

toTSeq :: [Term] -> Term
toTSeq [] = TRest
toTSeq [t] = TSeq t TEmpty
toTSeq (t:ts) = TSeq t (toTSeq ts)
