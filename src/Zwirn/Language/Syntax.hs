module Zwirn.Language.Syntax where

import Data.List (intercalate)
import Sound.Tidal.ID (ID)


type Var = String

type Name = String

type Position = ((Int,Int),(Int,Int))

-- sugary representation of patterns
data Term = TVar Position Var
          | TRest
          | TElong Term
          | TSeq [Term]
          | TStack [Term]
          | TAlt [Term]
          | TChoice Int [Term]
          | TMult Term Term
          | TDiv Term Term
          | TEuclid Term Term Term Term
          | TPoly Term Term
          | TLambda [Var] Term
          | TApp Term Term
          | TOp Name Term Term
          deriving (Eq, Show)

-- simple representation of patterns
data SimpleTerm = SVar (Maybe Position) Var
          | SRest
          | SElong SimpleTerm
          | SSeq [SimpleTerm]
          | SStack [SimpleTerm]
          | SChoice Int [SimpleTerm]
          | SMult SimpleTerm SimpleTerm
          | SDiv SimpleTerm SimpleTerm
          | SEuclid SimpleTerm SimpleTerm SimpleTerm SimpleTerm
          | SLambda Var SimpleTerm
          | SApp SimpleTerm SimpleTerm
          | SOp Name SimpleTerm SimpleTerm
          deriving (Eq, Show)

data Def = Let String [Var] Term deriving (Eq,Show)

data SimpleDef = LetS String SimpleTerm deriving (Eq,Show)

data Action = Exec ID Term | Def Def | Type Term | Show Term | Load String | Hydra Term deriving (Eq,Show)

displayTerm :: Term -> String
displayTerm (TVar _ x) = x
displayTerm (TRest) = "~"
displayTerm (TElong t) = displayTerm t  ++ "@"
displayTerm (TSeq ts) = "[" ++ (intercalate " " $ map displayTerm ts) ++ "]"
displayTerm (TAlt ts) = "<" ++ (intercalate " " $ map displayTerm ts) ++ ">"
displayTerm (TChoice _ ts) = "[" ++ (intercalate "|" $ map displayTerm ts) ++ "]"
displayTerm (TStack ts) = "(" ++ (intercalate "," $ map displayTerm ts) ++ ")"
displayTerm (TMult t1 t2) = displayTerm t1 ++ "*" ++ displayTerm t2
displayTerm (TDiv t1 t2) = displayTerm t1 ++ "/" ++ displayTerm t2
displayTerm (TEuclid t1 t2 t3 t4) = displayTerm t1 ++ "{" ++ displayTerm t2 ++ "," ++ displayTerm t3 ++ "," ++ displayTerm t4 ++ "}"
displayTerm (TPoly t1 t2) = displayTerm t1 ++ "%" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TOp n t1 t2) = "(" ++ displayTerm t1 ++ " " ++ n ++ " " ++ displayTerm t2 ++ ")"
displayTerm (TLambda vs t) = "(\\" ++ (intercalate " " vs) ++ " -> " ++ displayTerm t ++ ")"

simplify :: Term -> SimpleTerm
simplify (TVar p x) = SVar (Just p) x
simplify (TRest) = SRest
simplify (TElong t) = SElong (simplify t)
simplify (TSeq ts) = SSeq (map simplify ts)
simplify (TStack ts) = SStack (map simplify ts)
simplify (TChoice i ts) = SChoice i (map simplify ts)
simplify (TAlt ts) = SDiv (SSeq ss) (SVar Nothing (show $ length ss))
                   where ss = map simplify ts
simplify (TMult x y) = SMult (simplify x) (simplify y)
simplify (TDiv x y) = SDiv (simplify x) (simplify y)
simplify (TEuclid t1 t2 t3 t4) = SEuclid (simplify t1) (simplify t2) (simplify t3) (simplify t4)
simplify (TPoly (TSeq ts) n) = SMult (SDiv (SSeq ss) (SVar Nothing (show $ length ss))) (simplify n)
                   where ss = map simplify ts
simplify (TPoly x n) = SMult (simplify x) (simplify n)
simplify (TLambda [] t) = simplify t
simplify (TLambda (x:xs) t) = SLambda x (simplify $ TLambda xs t)
simplify (TApp x y) = SApp (simplify x) (simplify y)
simplify (TOp n x y) = SOp n (simplify x) (simplify y)


simplifyDef :: Def -> SimpleDef
simplifyDef (Let x vs t) = LetS x (simplify $ TLambda vs t)


-- representing the simple type system, might be useful for providing type signatures in definitions

type PolyName = String

data Type = Poly PolyName | Number | String | List Type | Func Type Type deriving Eq

data Class = Class String deriving Eq

data Annotation = Annotation [(Class, PolyName)] Type deriving Eq

instance Show Type where
  show (Poly x) = x
  show Number = "Number"
  show String = "String"
  show (List x) = "[" ++ show x ++ "]"
  show (Func x y) = show x ++ "->" ++ show y

instance Show Class where
  show (Class x) = x

instance Show Annotation where
  show (Annotation cs t) = "(" ++ intercalate "," (map (\(c,n) -> show c ++ " " ++ n) cs) ++ ") => " ++ show t
