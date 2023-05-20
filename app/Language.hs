module Language where

import Data.List (intercalate)


type Var = String

type Name = String

-- sugary representation of patterns
data Term = TVar Var
          | TRest
          | TElong Term
          | TSeq [Term]
          | TStack [Term]
          | TAlt [Term]
          | TChoice [Term]
          | TMult Term Term
          | TDiv Term Term
          | TPoly Term Term
          | TLambda [Var] Term
          | TApp Term Term
          | TOp Name Term Term
          deriving (Eq, Show)

-- simple representation of patterns
data Simple = SVar Var
          | SRest
          | SElong Simple
          | SSeq [Simple]
          | SStack [Simple]
          | SChoice [Simple]
          | SMult Simple Simple
          | SDiv Simple Simple
          | SLambda Var Simple
          | SApp Simple Simple
          | SOp Name Simple Simple
          deriving (Eq, Show)

data Def = Let String [Var] Term deriving (Eq,Show)

data SimpleDef = LetS String Simple deriving (Eq,Show)

data Action = Exec Term | Def Def | Type Term deriving (Eq,Show)

displayTerm :: Term -> String
displayTerm (TVar x) = x
displayTerm (TRest) = "~"
displayTerm (TElong t) = displayTerm t  ++ "@"
displayTerm (TSeq ts) = "[" ++ (intercalate " " $ map displayTerm ts) ++ "]"
displayTerm (TAlt ts) = "<" ++ (intercalate " " $ map displayTerm ts) ++ ">"
displayTerm (TChoice ts) = "[" ++ (intercalate "|" $ map displayTerm ts) ++ "]"
displayTerm (TStack ts) = "(" ++ (intercalate "," $ map displayTerm ts) ++ ")"
displayTerm (TMult t1 t2) = displayTerm t1 ++ "*" ++ displayTerm t2
displayTerm (TDiv t1 t2) = displayTerm t1 ++ "/" ++ displayTerm t2
displayTerm (TPoly t1 t2) = displayTerm t1 ++ "%" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TOp n t1 t2) = "(" ++ displayTerm t1 ++ " " ++ n ++ " " ++ displayTerm t2 ++ ")"
displayTerm (TLambda vs t) = "(\\" ++ (intercalate " " vs) ++ " -> " ++ displayTerm t ++ ")"

simplify :: Term -> Simple
simplify (TVar x) = SVar x
simplify TRest = SRest
simplify (TElong t) = SElong (simplify t)
simplify (TSeq ts) = SSeq (map simplify ts)
simplify (TStack ts) = SStack (map simplify ts)
simplify (TChoice ts) = SChoice (map simplify ts)
simplify (TAlt ts) = SDiv (SSeq ss) (SVar $ show $ length ss)
                   where ss = map simplify ts
simplify (TMult x y) = SMult (simplify x) (simplify y)
simplify (TDiv x y) = SDiv (simplify x) (simplify y)
simplify (TPoly (TSeq ts) n) = SMult (SDiv (SSeq ss) (SVar $ show $ length ss)) (simplify n)
                   where ss = map simplify ts
simplify (TPoly x n) = SMult (simplify x) (simplify n)
simplify (TLambda [] t) = simplify t
simplify (TLambda (x:xs) t) = SLambda x (simplify $ TLambda xs t)
simplify (TApp x y) = SApp (simplify x) (simplify y)
simplify (TOp n x y) = SOp n (simplify x) (simplify y)


simplifyDef :: Def -> SimpleDef
simplifyDef (Let x vs t) = LetS x (simplify $ TLambda vs t)
