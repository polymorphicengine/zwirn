module Language where

import Data.List (intercalate)


type Var = String

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
          | TLambda Var Term
          | TApp Term Term
          deriving (Eq, Show)


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
displayTerm (TLambda x t2) = "(\\" ++ x ++ "." ++ displayTerm t2 ++ ")"

-- church numeral stuff..

toChurch :: Int -> Term
toChurch n = TLambda "g" (TLambda "x" $ nApp n)

nApp :: Int -> Term
nApp 0 = TVar "x"
nApp n = TApp (TVar "g") (nApp $ n - 1)

fromChurch :: Term -> Term
fromChurch (TLambda "g" (TLambda "x" t)) = case nAppRev t of
                                                    Just n -> TInt n
                                                    Nothing -> (TLambda "g" (TLambda "x" t))
fromChurch (TSeq t1 t2) = TSeq (fromChurch t1) (fromChurch t2)
fromChurch (TAlt t1 t2) = TAlt (fromChurch t1) (fromChurch t2)
fromChurch (TStack t1 t2) = TStack (fromChurch t1) (fromChurch t2)
fromChurch (TMult t1 t2) = TMult (fromChurch t1) (fromChurch t2)
fromChurch (TDiv t1 t2) = TDiv (fromChurch t1) (fromChurch t2)
fromChurch (TApp t1 t2) = TApp (fromChurch t1) (fromChurch t2)
fromChurch (TSub t) = TSub (fromChurch t)
fromChurch (TLambda x t) = TLambda x (fromChurch t)
fromChurch x = x

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
