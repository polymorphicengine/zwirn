module Language where

import Data.List (intercalate)


type Var = String

data Pat = PVar Var
         | PSeq Var Var
         | PNil
         | PDiv Var Var
         | PMult Var Var
         deriving (Eq, Show)

data Term = TVar Var
          | TInt Int
          | TRest
          | TEmpty
          | TSeq Term Term
          | TStack Term Term
          | TAlt Term Term
          | TSub Term
          | TMult Term Term
          | TDiv Term Term
          | TLambda [(Pat,Term)]
          | TApp Term Term
          deriving (Eq, Show)

displayPat :: Pat -> String
displayPat (PVar x) = x
displayPat (PSeq x y) = "(" ++ x ++ " " ++ y ++ ")"
displayPat (PNil) = "nil"
displayPat (PDiv x n) = x ++ "/" ++ n
displayPat (PMult x n) = x ++ "*" ++ n


displayTerm :: Term -> String
displayTerm (TVar x) = x
displayTerm (TInt i) = show i
displayTerm (TRest) = "~"
displayTerm TEmpty = ""
displayTerm t@(TSeq _ _) = "(" ++ (intercalate " " $ map displayTerm (getTSeq t)) ++ ")"
displayTerm t@(TAlt _ _) = "<" ++ (intercalate " " $ map displayTerm (removeEmpty $ getTAlt t)) ++ ">"
displayTerm (TSub t) = "[" ++ displayTerm t ++ "]"
displayTerm (TStack t1 t2) = displayTerm t1 ++ "," ++ displayTerm t2
displayTerm (TMult t1 t2) = displayTerm t1 ++ "*" ++ displayTerm t2
displayTerm (TDiv t1 t2) = displayTerm t1 ++ "/" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TLambda ps) = "(\\" ++ (intercalate "|" $ map (\(p,t) -> displayPat p ++ "." ++ displayTerm t) ps) ++ ")"

patToHaskell :: Pat -> String
patToHaskell (PVar x) = x
patToHaskell (PSeq x y) = "(TSeq " ++ x ++ " " ++ y ++ ")"
patToHaskell (PNil) = "TEmpty"
patToHaskell (PDiv x n) = "(TDiv " ++ x ++ " " ++ n ++ ")"
patToHaskell (PMult x n) = "(TMult " ++ x ++ " " ++ n ++ ")"

toHaskell :: Term -> String
toHaskell (TVar x) = x
toHaskell (TInt i) = "(TInt " ++ show i ++ ")"
toHaskell TRest = "TRest"
toHaskell TEmpty = "TEmpty"
toHaskell (TSeq t1 t2) = "(TSeq " ++ toHaskell t1 ++ " " ++ toHaskell t2 ++ ")"
toHaskell (TAlt t1 t2) = "(TAlt " ++ toHaskell t1 ++ " " ++ toHaskell t2 ++ ")"
toHaskell (TSub t) = "(TSub " ++ toHaskell t ++ ")"
toHaskell (TStack t1 t2) = "(TStack " ++ toHaskell t1 ++ " " ++ toHaskell t2 ++ ")"
toHaskell (TMult t1 t2) = "(TMult " ++ toHaskell t1 ++ " " ++ toHaskell t2 ++ ")"
toHaskell (TDiv t1 t2) = "(TDiv " ++ toHaskell t1 ++ " " ++ toHaskell t2 ++ ")"
toHaskell (TApp t1 t2) = "(" ++ toHaskell t1 ++ " " ++ toHaskell t2 ++ ")"
toHaskell (TLambda ps) = "(\\" ++ (intercalate "|" $ map (\(p,t) -> patToHaskell p ++ "->" ++ toHaskell t) ps) ++ ")"

patVars :: Pat -> [Var]
patVars (PVar x) = [x]
patVars (PSeq x y) = [x,y]
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
fromChurch (TAlt t1 t2) = TAlt (fromChurch t1) (fromChurch t2)
fromChurch (TStack t1 t2) = TStack (fromChurch t1) (fromChurch t2)
fromChurch (TMult t1 t2) = TMult (fromChurch t1) (fromChurch t2)
fromChurch (TDiv t1 t2) = TDiv (fromChurch t1) (fromChurch t2)
fromChurch (TApp t1 t2) = TApp (fromChurch t1) (fromChurch t2)
fromChurch (TSub t) = TSub (fromChurch t)
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


getTAlt :: Term -> [Term]
getTAlt (TAlt t1 t2) = t1:(getTAlt t2)
getTAlt t = [t]

toTAlt :: [Term] -> Term
toTAlt [] = error "Not possible"
toTAlt [t] = TAlt t TEmpty
toTAlt (t:ts) = TAlt t (toTAlt ts)

removeEmpty :: [Term] -> [Term]
removeEmpty = concatMap (\t -> if t == TEmpty then [] else [t])
