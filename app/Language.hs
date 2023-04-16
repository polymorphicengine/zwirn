module Language where

import Data.List (intercalate)


type Var = String

data Term = TVar Var
          | TInt Int
          | TRest
          | TElong Term
          | TSeq [Term]
          | TStack [Term]
          | TMult Term Term
          | TDiv Term Term
          | TLambda Var Term
          | TApp Term Term
          deriving (Eq, Show)

displayTerm :: Term -> String
displayTerm (TVar x) = x
displayTerm (TInt i) = show i
displayTerm (TRest) = "~"
displayTerm (TElong t) = displayTerm t  ++ "@"
displayTerm (TSeq ts) = "[" ++ (intercalate " " $ map displayTerm ts) ++ "]"
displayTerm (TStack ts) = "(" ++ (intercalate "," $ map displayTerm ts) ++ ")"
displayTerm (TMult t1 t2) = displayTerm t1 ++ "*" ++ displayTerm t2
displayTerm (TDiv t1 t2) = displayTerm t1 ++ "/" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TLambda v t) = "(\\" ++ v ++ " -> " ++ displayTerm t ++ ")"
