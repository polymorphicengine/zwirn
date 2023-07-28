module Zwirn.Language.Pretty
    ( displayTerm
    ) where

import Zwirn.Language.Syntax

import Data.List (intercalate)
import Data.Text (unpack)

displayTerm :: Term -> String
displayTerm (TVar _ x) = show x
displayTerm (TRest) = "~"
displayTerm (TElong t (Just i)) = displayTerm t  ++ "@" ++ show i
displayTerm (TElong t Nothing) = displayTerm t  ++ "@"
displayTerm (TRepeat t (Just i)) = displayTerm t  ++ "!" ++ show i
displayTerm (TRepeat t Nothing) = displayTerm t  ++ "!"
displayTerm (TSeq ts) = "[" ++ (intercalate " " $ map displayTerm ts) ++ "]"
displayTerm (TAlt ts) = "<" ++ (intercalate " " $ map displayTerm ts) ++ ">"
displayTerm (TChoice _ ts) = "[" ++ (intercalate "|" $ map displayTerm ts) ++ "]"
displayTerm (TStack ts) = "(" ++ (intercalate "," $ map displayTerm ts) ++ ")"
displayTerm (TEuclid t1 t2 t3 (Just t4)) = displayTerm t1 ++ "{" ++ displayTerm t2 ++ "," ++ displayTerm t3 ++ "," ++ displayTerm t4 ++ "}"
displayTerm (TEuclid t1 t2 t3 Nothing) = displayTerm t1 ++ "{" ++ displayTerm t2 ++ "," ++ displayTerm t3 ++ "}"
displayTerm (TPoly t1 t2) = displayTerm t1 ++ "%" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TInfix t1 n t2) = "(" ++ displayTerm t1 ++ " " ++ unpack n ++ " " ++ displayTerm t2 ++ ")"
displayTerm (TLambda vs t) = "(\\" ++ (intercalate " " $ map unpack vs) ++ " -> " ++ displayTerm t ++ ")"
