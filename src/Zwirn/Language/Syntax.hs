{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Syntax where

import Data.List (intercalate)
import Sound.Tidal.ID (ID)

import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text


type Var = Text

type OperatorSymbol = Text

-- type Position = ((Int,Int),(Int,Int))

data Position = Pos { pLine :: Int
                    , pStart :: Int
                    , pEnd :: Int
                    , pEditor :: Int
                    } deriving (Eq, Show)

-- sugary representation of patterns
data Term = TVar Position Text
          | TRest
          | TElong Term Int
          | TRepeat Term Int
          | TSeq [Term]
          | TStack [Term]
          | TAlt [Term]
          | TChoice Int [Term]
          | TEuclid Term Term Term (Maybe Term)
          | TPoly Term Term
          | TLambda [Text] Term
          | TApp Term Term
          | TInfix Term Text Term
          deriving (Eq, Show)

data Def = Let Text [Text] Term deriving (Eq,Show)

data Action = Stream Text Term
            | Def Def
            | Type Term
            | Show Term
            | Load Text
            | JS Term
            deriving (Eq,Show)

-- simple representation of patterns
data SimpleTerm = SVar (Maybe Position) Var
          | SRest
          | SElong SimpleTerm
          | SSeq [SimpleTerm]
          | SStack [SimpleTerm]
          | SChoice Int [SimpleTerm]
          | SEuclid SimpleTerm SimpleTerm SimpleTerm SimpleTerm
          | SLambda Var SimpleTerm
          | SApp SimpleTerm SimpleTerm
          | SOp SimpleTerm OperatorSymbol SimpleTerm
          deriving (Eq, Show)


data SimpleDef = LetS Var SimpleTerm deriving (Eq,Show)

displayTerm :: Term -> String
displayTerm (TVar _ x) = show x
displayTerm (TRest) = "~"
displayTerm (TElong t i) = displayTerm t  ++ "@" ++ show i
displayTerm (TSeq ts) = "[" ++ (intercalate " " $ map displayTerm ts) ++ "]"
displayTerm (TAlt ts) = "<" ++ (intercalate " " $ map displayTerm ts) ++ ">"
displayTerm (TChoice _ ts) = "[" ++ (intercalate "|" $ map displayTerm ts) ++ "]"
displayTerm (TStack ts) = "(" ++ (intercalate "," $ map displayTerm ts) ++ ")"
displayTerm (TEuclid t1 t2 t3 (Just t4)) = displayTerm t1 ++ "{" ++ displayTerm t2 ++ "," ++ displayTerm t3 ++ "," ++ displayTerm t4 ++ "}"
displayTerm (TPoly t1 t2) = displayTerm t1 ++ "%" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TInfix t1 n t2) = "(" ++ displayTerm t1 ++ " " ++ unpack n ++ " " ++ displayTerm t2 ++ ")"
displayTerm (TLambda vs t) = "(\\" ++ (intercalate " " $ map unpack vs) ++ " -> " ++ displayTerm t ++ ")"

simplify :: Term -> SimpleTerm
simplify (TVar p x) = SVar (Just p) x
simplify (TRest) = SRest
simplify (TElong t i) = SElong (simplify t) ---FIXME
simplify (TSeq ts) = SSeq (map simplify ts)
simplify (TStack ts) = SStack (map simplify ts)
simplify (TChoice i ts) = SChoice i (map simplify ts)
simplify (TAlt ts) = SOp (SSeq ss) "/" (SVar Nothing (pack $ show $ length ss))
                   where ss = map simplify ts
simplify (TEuclid t1 t2 t3 (Just t4)) = SEuclid (simplify t1) (simplify t2) (simplify t3) (simplify t4) --FIXME
simplify (TPoly (TSeq ts) n) = SOp (SOp (SSeq ss) "/" (SVar Nothing (pack $ show $ length ss))) "*" (simplify n)
                   where ss = map simplify ts
simplify (TPoly x n) = SOp (simplify x) "*" (simplify n)
simplify (TLambda [] t) = simplify t
simplify (TLambda (x:xs) t) = SLambda x (simplify $ TLambda xs t)
simplify (TApp x y) = SApp (simplify x) (simplify y)
simplify (TInfix x op y) = SOp (simplify x) op (simplify y)


simplifyDef :: Def -> SimpleDef
simplifyDef (Let x vs t) = LetS x (simplify $ TLambda vs t)
