module Language where

import Data.List (intercalate)


type Var = String

data Pat = PVar Var
         | PEmpty
         | PSeq Pat Pat
         | PStack Pat Pat
         | PDiv Pat Pat
         | PMult Pat Pat
         deriving (Eq, Show)

data Term = TVar Var
          | TInt Int
          | TRest
          | TEmpty
          | TSeq Term Term
          | TStack Term Term
          | TMult Term Term
          | TDiv Term Term
          | TLambda [(Pat,Term)]
          | TApp Term Term
          deriving (Eq, Show)

data TermF = FVar Var
          | FInt Int
          | FRest
          | FEmpty
          | FSeq TermF TermF
          | FStack TermF TermF
          | FMult TermF TermF
          | FDiv TermF TermF
          | FLambda (TermF -> TermF)

displayPat :: Pat -> String
displayPat (PVar x) = x
displayPat (PSeq x y) = "(" ++ displayPat x ++ " " ++ displayPat y ++ ")"
displayPat (PStack x y) = "(" ++ displayPat x ++ "," ++ displayPat y ++ ")"
displayPat (PEmpty) = "nil"
displayPat (PDiv x n) = displayPat x ++ "/" ++ displayPat n
displayPat (PMult x n) = displayPat x ++ "*" ++ displayPat n

displayTerm :: Term -> String
displayTerm (TVar x) = x
displayTerm (TInt i) = show i
displayTerm (TRest) = "~"
displayTerm TEmpty = ""
displayTerm t@(TSeq _ _) = "(" ++ (intercalate " " $ map displayTerm (getTSeq t)) ++ ")"
displayTerm (TStack t1 t2) = displayTerm t1 ++ "," ++ displayTerm t2
displayTerm (TMult t1 t2) = displayTerm t1 ++ "*" ++ displayTerm t2
displayTerm (TDiv t1 t2) = displayTerm t1 ++ "/" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TLambda ps) = "(\\" ++ (intercalate "|" $ map (\(p,t) -> displayPat p ++ "." ++ displayTerm t) ps) ++ ")"

patVars :: Pat -> [Var]
patVars (PVar x) = [x]
patVars (PSeq x y) = patVars x ++ patVars y
patVars _ = []

-- church numeral stuff..

toChurch :: Int -> Term
toChurch n = TLambda [(PVar "g",TLambda [(PVar "x", nApp n)])]

nApp :: Int -> Term
nApp 0 = TVar "x"
nApp n = TApp (TVar "g") (nApp $ n - 1)

fromChurch :: Term -> Term
fromChurch (TLambda [(PVar "g", TLambda [(PVar "x", t)])]) = case nAppRev t of
                                                    Just n -> TInt n
                                                    Nothing -> TLambda [(PVar "g", TLambda [(PVar "x", t)])]
fromChurch (TLambda [(PVar "x", TLambda [(PVar "y",TVar "x")])]) = TVar "t"
fromChurch (TLambda [(PVar "x", TLambda [(PVar "y",TVar "y")])]) = TVar "f"
fromChurch (TSeq t1 t2) = TSeq (fromChurch t1) (fromChurch t2)
fromChurch (TStack t1 t2) = TStack (fromChurch t1) (fromChurch t2)
fromChurch (TMult t1 t2) = TMult (fromChurch t1) (fromChurch t2)
fromChurch (TDiv t1 t2) = TDiv (fromChurch t1) (fromChurch t2)
fromChurch (TApp t1 t2) = TApp (fromChurch t1) (fromChurch t2)
fromChurch (TLambda ts) = TLambda $ mapTerm fromChurch ts
fromChurch x = x

mapTerm :: (Term -> Term) -> [(Pat,Term)] -> [(Pat,Term)]
mapTerm f = map (\(p,t) -> (p, f t))

nAppRev :: Term -> Maybe Int
nAppRev (TVar "x") = Just 0
nAppRev (TApp (TVar "g") t) = fmap (+1) (nAppRev t)
nAppRev _ = Nothing

--Seq // Alt Stuff

tSeqLen :: Term -> Int
tSeqLen (TSeq _ t1) = 1 + tSeqLen t1
tSeqLen _ = 1

getTSeq :: Term -> [Term]
getTSeq (TSeq t1 t2) = t1:(getTSeq t2)
getTSeq t = [t]


toTSeq :: [Term] -> Term
toTSeq [] = TRest
toTSeq [t] = t
toTSeq (t:ts) = TSeq t (toTSeq ts)

getFSeq :: TermF -> [TermF]
getFSeq (FSeq t1 t2) = t1:(getFSeq t2)
getFSeq t = [t]


toFSeq :: [TermF] -> TermF
toFSeq [] = FRest
toFSeq [t] = t
toFSeq (t:ts) = FSeq t (toFSeq ts)

removeEmpty :: [Term] -> [Term]
removeEmpty = concatMap (\t -> if t == TEmpty then [] else [t])
