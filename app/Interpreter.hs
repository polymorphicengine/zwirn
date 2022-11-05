module Interpreter where

import Language

import Data.Map as Map (Map, fromList, lookup)

type Env = Map String Term

envTry :: Env
envTry = fromList [("fast", TLambda "x" (TLambda "y" (TMult (TVar "y") (TVar "x"))))
                  ,("slow", TLambda "x" (TLambda "y" (TDiv (TVar "y") (TVar "x"))))
                  ,("t", TLambda "x" (TLambda "y" (TVar "x")))
                  ,("f", TLambda "x" (TLambda "y" (TVar "y")))
                  ,("and", TLambda "p" (TLambda "q" (TApp (TApp (TVar "p") (TVar "q")) (TVar "p"))))
                  ,("id", TLambda "x" (TVar "x"))
                  ,("if", TLambda "p" (TLambda "a" (TLambda "b" (TApp ( (TApp (TVar "p") (TVar "a"))) (TVar "b")))))
                  ,("while", TLambda "p" (TLambda "h" (TLambda "x" (TApp (TApp (TApp (TVar "if") (TVar "p")) (TApp (TVar "h") (TVar "x"))) (TVar "x")))))
                  ,("Y", (TLambda "g" ( (TApp ( (TLambda "x" (TApp (TVar "g") ( (TApp (TVar "x") (TVar "x")))))) ( (TLambda "z" (TApp (TVar "g") ( (TApp (TVar "z") (TVar "z"))))))))))
                  ,("succ", TLambda "n" (TLambda "g" (TLambda "x" (TApp (TVar "g") ((TApp (TApp (TVar "n") (TVar "g")) (TVar "x")))))))
                  ,("add", TLambda "m" (TLambda "n" (TApp (TApp (TVar "m") (TVar "succ")) (TVar "n"))))
                  ,("pred", TLambda "n" (TLambda "g" (TLambda "x" (TApp (TApp (TApp (TVar "n") (TLambda "j" (TLambda "h" (TApp (TVar "h") (TApp (TVar "j") (TVar "g")))))) (TLambda "u" (TVar "x"))) (TLambda "u" (TVar "u"))))))
                  ,("sub", TLambda "m" (TLambda "n" (TApp ( (TApp (TVar "n") (TVar "pred"))) (TVar "m"))) )
                  ,("leq", TLambda "m" (TLambda "n" (TApp (TVar "iszero") (TApp (TApp (TVar "sub") (TVar "m")) (TVar "n")))))
                  ,("eq", TLambda "m" (TLambda "n" (TApp (TApp (TVar "and") (TApp (TApp (TVar "leq") (TVar "m")) (TVar "n"))) (TApp (TApp (TVar "leq") (TVar "n")) (TVar "m")))))
                  ,("iszero", TLambda "n" (TApp ( (TApp (TVar "n") (TLambda "x" (TVar "f")))) (TVar "t")))
                  ,("eq", TLambda "m" (TLambda "n" (TApp (TApp (TVar "and") (TApp (TApp (TVar "leq") (TVar "m")) (TVar "n"))) (TApp (TApp (TVar "leq") (TVar "n")) (TVar "m")))))
                  ,("loop", TLambda "x" (TApp (TLambda "h" (TSeq (TVar "x") (TApp (TVar "h") (TVar "h")))) (TLambda "h" (TSeq (TVar "x") (TApp (TVar "h") (TVar "h"))))))
                  ,("revrun", TApp (TVar "Y") (TLambda "h" (TLambda "n" (TApp (TApp (TApp (TVar "if") (TApp (TVar "iszero") (TApp (TVar "pred") (TVar "n")))) (TInt 0)) (TSeq (TApp (TVar "pred") (TVar "n")) (TApp (TVar "h") (TApp (TVar "pred") (TVar "n"))))))))
                  ,("run", TApp (TApp (TVar "Y") (TLambda "h" (TLambda "k" (TLambda "n" (TApp (TApp (TApp (TVar "if") (TApp (TApp (TVar "eq") (TVar "k")) (TApp (TVar "pred") (TVar "n")))) (TApp (TVar "pred") (TVar "n"))) (TSeq (TVar "k") (TApp (TApp (TVar "h") (TApp (TVar "succ") (TVar "k"))) (TVar "n")))))))) (TInt 0))
                  ]

subs :: Var -> Term -> Term -> Term
subs x (TVar y) t = if x == y then t else (TVar y)
subs _ t@(TInt _) _ = t
subs _ TRest _ = TRest
subs _ TEmpty _ = TEmpty
subs x (TSeq t1 t2) t = TSeq (subs x t1 t) (subs x t2 t)
subs x (TAlt t1 t2) t = TAlt (subs x t1 t) (subs x t2 t)
subs x (TStack t1 t2) t = TStack (subs x t1 t) (subs x t2 t)
subs x (TMult t1 t2) t = TMult (subs x t1 t) (subs x t2 t)
subs x (TDiv t1 t2) t = TDiv (subs x t1 t) (subs x t2 t)
subs x (TApp t1 t2) t = TApp (subs x t1 t) (subs x t2 t)
subs x (TSub t1) t = TSub (subs x t1 t)
subs x (TLambda y t1) t = if x /= y then TLambda y (subs x t1 t) else TLambda y t1

-- the idea is that the structure will always come from the right term and the values in the left term will be matched against them
applySeqToSeq :: Term -> Term -> Term
applySeqToSeq t1 t2 = toTSeq $ map (\i -> zs!!i) [x*l2 | x <- [0..n2-1]]
                    where s1 = getTSeq t1
                          s2 = getTSeq t2
                          n1 = length s1
                          n2 = length s2
                          l = lcm n1 n2
                          l1 = div l n1
                          l2 = div l n2
                          f1 = concatMap (\x -> take l1 $ repeat x) s1
                          f2 = concatMap (\x -> take l2 $ repeat x) s2
                          zs = zipWith (\x y -> TApp x y) f1 f2

-- here the structure basically comes from both sides, so the total number of cycles in the result is the lcm of both individual patterns
applyAltToAlt :: Term -> Term -> Term
applyAltToAlt t1 t2 = toTAlt $ zs
                    where s1 = removeEmpty $ getTAlt t1
                          s2 = removeEmpty $ getTAlt t2
                          n1 = length s1
                          n2 = length s2
                          l = lcm n1 n2
                          l1 = div l n1
                          l2 = div l n2
                          f1 = concat $ replicate l1 s1
                          f2 = concat $ replicate l2 s2
                          zs = zipWith (\x y -> TApp x y) f1 f2

-- in the case of multiplication or division the application is only propagated to the value of the term, not the modifiers(tail fs)

isFree :: Var -> Term -> Bool
isFree x (TVar y) = x /= y
isFree x (TSub t) = isFree x t
isFree x (TSeq t1 t2) = (isFree x t1) && (isFree x t2)
isFree x (TAlt t1 t2) = (isFree x t1) && (isFree x t2)
isFree x (TStack t1 t2) = (isFree x t1) && (isFree x t2)
isFree x (TMult t1 t2) = (isFree x t1) && (isFree x t2)
isFree x (TDiv t1 t2) = (isFree x t1) && (isFree x t2)
isFree x (TApp t1 t2) = (isFree x t1) && (isFree x t2)
isFree x (TLambda y t) =  x /= y && isFree x t
isFree _ _ = False


isReduced :: Term -> Bool
isReduced (TVar x) = case Map.lookup x envTry of
                              Just _ -> False
                              Nothing -> True
isReduced (TInt _) = False
isReduced (TRest) = True
isReduced TEmpty = True
isReduced (TSub t) = isReduced t
isReduced (TSeq t1 t2) = isReduced t1 && isReduced t2
isReduced (TAlt t1 t2) = isReduced t1 && isReduced t2
isReduced (TStack t1 t2) = isReduced t1 && isReduced t2
isReduced (TMult t1 t2) = isReduced t1 && isReduced t2
isReduced (TDiv t1 t2) = isReduced t1 && isReduced t2
isReduced (TLambda _ t) = isReduced t
isReduced (TApp (TLambda _ _) _) = False
isReduced (TApp (TSeq _ _) _) = False
isReduced (TApp (TMult _ _) _) = False
isReduced (TApp (TDiv _ _) _) = False
isReduced (TApp (TSub _) _) = False
isReduced (TApp t1 t2) = isReduced t1 && isReduced t2

reduce :: Term -> Term
reduce (TApp f t) = case reduce f of
                            TLambda x t1 -> subs x t1 t
                            TSub t1 -> TSub (TApp t1 t)
                            TMult t1 t2 -> TMult (TApp t1 t) t2
                            TDiv t1 t2 -> TDiv (TApp t1 t) t2
                            t1@(TSeq _ _) -> applySeqToSeq t1 t
                            t1@(TAlt _ _) -> applyAltToAlt t1 t
                            TEmpty -> TEmpty
                            t2 -> TApp t2 (reduce t)
reduce (TSub t) = TSub (reduce t)
reduce (TVar x) = case Map.lookup x envTry of
                              Just t -> t
                              Nothing -> TVar x
reduce (TInt i) = toChurch i
reduce (TSeq t1 t2) = TSeq (reduce t1) (reduce t2)
reduce (TAlt t1 t2) = TAlt (reduce t1) (reduce t2)
reduce (TMult t1 t2) = TMult (reduce t1) (reduce t2)
reduce (TDiv t1 t2) = TDiv (reduce t1) (reduce t2)
reduce (TLambda x t) = TLambda x (reduce t)
reduce  t = t

_alphaConv :: [Var] -> Term -> Term
_alphaConv vs (TLambda x t) = case isFree x t && (not $ elem x vs) of
                                            True -> t
                                            False -> TLambda x (_alphaConv (x:vs) t)
_alphaConv vs (TSub t) = TSub $ _alphaConv vs t
_alphaConv vs (TSeq t1 t2) = TSeq (_alphaConv vs t1) (_alphaConv vs t2)
_alphaConv vs (TAlt t1 t2) = TAlt (_alphaConv vs t1) (_alphaConv vs t2)
_alphaConv vs (TStack t1 t2) = TStack (_alphaConv vs t1) (_alphaConv vs t2)
_alphaConv vs (TMult t1 t2) = TMult (_alphaConv vs t1) (_alphaConv vs t2)
_alphaConv vs (TDiv t1 t2) = TDiv (_alphaConv vs t1) (_alphaConv vs t2)
_alphaConv _ t = t

alphaConv :: Term -> Term
alphaConv = _alphaConv []


reduceMany :: Term -> Term
reduceMany t = case isReduced t of
                    True -> t
                    False -> reduceMany $ reduce t
