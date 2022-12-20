module Interpreter where

import Language

import Data.Map as Map (Map, fromList, lookup)

type Env = Map String Term

envTry :: Env
envTry = fromList [("t",TLambda [(PVar "x",TLambda [(PVar "y",TVar "x")])])
                  ,("f",TLambda [(PVar "x",TLambda [(PVar "y",TVar "y")])])
                  ,("and",TLambda [(PVar "p",TLambda [(PVar "q",TApp (TApp (TVar "p") (TVar "q")) (TVar "p"))])])
                  ,("or",TLambda [(PVar "p",TLambda [(PVar "q",TApp (TApp (TVar "p") (TVar "p")) (TVar "q"))])])
                  ,("not",TLambda [(PVar "p",TApp (TApp (TVar "p") (TVar "f")) (TVar "t"))])
                  ,("if",TLambda [(PVar "p",TLambda [(PVar "a",TLambda [(PVar "b",TApp (TApp (TVar "p") (TVar "a")) (TVar "b"))])])])
                  ,("succ",TLambda [(PVar "n",TLambda [(PVar "g",TLambda [(PVar "x",TApp (TVar "g") (TApp (TApp (TVar "n") (TVar "g")) (TVar "x")))])])])
                  ,("add",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "m") (TVar "succ")) (TVar "n"))])])
                  ,("mult",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "m") (TApp (TVar "add") (TVar "n"))) (TInt 0))])])
                  ,("pow",TLambda [(PVar "b",TLambda [(PVar "e",TApp (TVar "e") (TVar "b"))])])
                  ,("pred",TLambda [(PVar "n",TLambda [(PVar "g",TLambda [(PVar "x",TApp (TApp (TApp (TVar "n") (TLambda [(PVar "j",TLambda [(PVar "h",TApp (TVar "h") (TApp (TVar "j") (TVar "g")))])])) (TLambda [(PVar "u",TVar "x")])) (TLambda [(PVar "u",TVar "u")]))])])])
                  ,("sub",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "n") (TVar "pred")) (TVar "m"))])])
                  ,("iszero",TLambda [(PVar "n",TApp (TApp (TVar "n") (TLambda [(PVar "x",TVar "f")])) (TVar "t"))])
                  ,("leq",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TVar "iszero") (TApp (TApp (TVar "sub") (TVar "m")) (TVar "n")))])])
                  ,("eq",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "and") (TApp (TApp (TVar "leq") (TVar "m")) (TVar "n"))) (TApp (TApp (TVar "leq") (TVar "n")) (TVar "m")))])])
                  ,("Y",TLambda [(PVar "func",TApp (TLambda [(PVar "arg",TApp (TVar "func") (TApp (TVar "arg") (TVar "arg")))]) (TLambda [(PVar "arg",TApp (TVar "func") (TApp (TVar "arg") (TVar "arg")))]))])
                  ,("Z",TLambda [(PVar "func",TApp (TLambda [(PVar "arg",TApp (TVar "func") (TLambda [(PVar "v",TApp (TApp (TVar "arg") (TVar "arg")) (TVar "v"))]))]) (TLambda [(PVar "arg",TApp (TVar "func") (TLambda [(PVar "v",TApp (TApp (TVar "arg") (TVar "arg")) (TVar "v"))]))]))])
                  ,("flip",TLambda [(PVar "h",TLambda [(PVar "x",TLambda [(PVar "y",TApp (TApp (TVar "h") (TVar "y")) (TVar "x"))])])])
                  ,("force",TLambda [(PSeq "x" "xs",TSeq (TVar "x") (TVar "xs")),(PVar "x",TVar "x")])
                  ,("isseq",TLambda [(PSeq "x" "xs",TVar "t"),(PVar "x",TVar "f")])
                  ,("seqhead",TLambda [(PSeq "x" "xs",TVar "x"),(PVar "x",TVar "x")])
                  ,("seqtail",TLambda [(PSeq "x" "xs",TVar "xs"),(PVar "x",TVar "x")])
                  ,("fast",TLambda [(PVar "n",TLambda [(PVar "x",TMult (TVar "x") (TVar "n"))])])
                  ,("slow",TLambda [(PVar "n",TLambda [(PVar "x",TDiv (TVar "x") (TVar "n"))])])
                  ,("revrun",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PVar "n",TApp (TApp (TApp (TVar "if") (TApp (TVar "iszero") (TApp (TVar "pred") (TVar "n")))) (TInt 0)) (TSeq (TApp (TVar "pred") (TVar "n")) (TApp (TVar "h") (TApp (TVar "pred") (TVar "n")))))])]))
                  ,("seqlen",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PSeq "y" "ys",TApp (TVar "succ") (TApp (TVar "h") (TVar "ys"))),(PVar "y",TInt 1)])]))
                  ,("seqdrop",TLambda [(PVar "n",TLambda [(PVar "y",TApp (TApp (TVar "n") (TVar "seqtail")) (TVar "y"))])])
                  ,("seqtake",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PVar "n",TLambda [(PSeq "y" "ys",TApp (TApp (TApp (TVar "if") (TApp (TApp (TVar "leq") (TVar "n")) (TInt 1))) (TVar "y")) (TSeq (TVar "y") (TApp (TApp (TVar "h") (TApp (TVar "pred") (TVar "n"))) (TVar "ys")))),(PVar "y",TVar "y")])])]))
                  ,("seqfront",TLambda [(PVar "x",TApp (TApp (TVar "seqtake") (TApp (TVar "pred") (TApp (TVar "seqlen") (TVar "x")))) (TVar "x"))])
                  ,("seqlast",TLambda [(PVar "x",TApp (TApp (TApp (TVar "seqlen") (TVar "x")) (TVar "seqtail")) (TVar "x"))])
                  ,("rot",TLambda [(PVar "n",TLambda [(PVar "y",TApp (TApp (TVar "n") (TLambda [(PVar "z",TSeq (TApp (TVar "seqlast") (TVar "z")) (TApp (TVar "seqfront") (TVar "z")))])) (TVar "y"))])])
                  ,("replicate",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PVar "k",TLambda [(PVar "x",TApp (TApp (TApp (TVar "if") (TApp (TVar "iszero") (TApp (TVar "pred") (TVar "k")))) (TVar "x")) (TSeq (TVar "x") (TApp (TApp (TVar "h") (TApp (TVar "pred") (TVar "k"))) (TVar "x"))))])])]))
                  ,("squeeze",TLambda [(PVar "h",TLambda [(PVar "x",TLambda [(PVar "y",TApp (TApp (TApp (TApp (TVar "replicate") (TApp (TVar "seqlen") (TVar "x"))) (TVar "h")) (TVar "x")) (TApp (TApp (TVar "replicate") (TApp (TVar "seqlen") (TVar "x"))) (TVar "y")))])])])
                  ,("rev",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PSeq "y" "ys",TSeq (TApp (TVar "h") (TVar "ys")) (TVar "y")),(PVar "z",TVar "z")])]))
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
subs x (TLambda ps) t = TLambda $ zip (map (\(p,_) -> p) ps) (map (subsPat x t) ps)

subsPat :: Var -> Term -> (Pat,Term) -> Term
subsPat x t (p, t1) = if not $ elem x (patVars p) then subs x t1 t else t1

matches :: Pat -> Term -> Bool
matches (PSeq _ _) (TSeq _ _) = True
matches (PSeq _ _) _ = False
matches PNil TEmpty = True
matches _ _ = True

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
--isFree x (TLambda ps) =  map (\(p,_) -> and $ map (\y -> x /= y) patVars p) ps && and $ map (\(_,t) -> isFree x t) ps
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
isReduced (TLambda ts) = and $ map (\(_,t) ->  isReduced t) ts
isReduced (TApp (TLambda ts) (TVar _)) = case ts of
                                            [(PVar _, _)] -> False
                                            _ -> True
isReduced (TApp (TLambda ts) t) = and $ map (\(p,_) -> not $ matches p t) ts
isReduced (TApp (TSeq _ _) _) = False
isReduced (TApp (TMult _ _) _) = False
isReduced (TApp (TDiv _ _) _) = False
isReduced (TApp (TSub _) _) = False
isReduced (TApp t1 t2) = isReduced t1 && isReduced t2

getMatching :: [(Pat,Term)] -> Term -> Maybe (Term, [(Var,Term)])
getMatching [] _ = Nothing
getMatching ((p,t1):ps) t = case matches p t of
                              True -> Just $ (t1, zip (patVars p) (splitMatch p t))
                              False -> getMatching ps t

splitMatch :: Pat -> Term -> [Term]
splitMatch (PVar _) t = [t]
splitMatch (PSeq _ _) (TSeq t1 t2) = [t1,t2]
splitMatch _ t = [t]

getPats :: [(Pat,Term)] -> [Pat]
getPats = map fst

matchesOnlyDefault :: [Pat] -> Term -> Bool
matchesOnlyDefault ps t = and $ map (\p -> not $ matches p t) (removeDefault ps)

removeDefault :: [Pat] -> [Pat]
removeDefault [] = []
removeDefault ((PVar _):ps) = removeDefault ps
removeDefault (p:ps) = p:(removeDefault ps)

getDefault :: [(Pat,Term)] -> Maybe (Var,Term)
getDefault [] = Nothing
getDefault ((PVar x, t):_) = Just (x,t)
getDefault (_:ps) = getDefault ps

reduce :: Term -> Term
reduce (TApp f t) = case reduce f of
                            -- TLambda ts -> case k of -- eager eval
                            --                 TVar _ -> TApp (TLambda ts) k
                            --                 _ -> case getMatching ts k of
                            --                                   Nothing -> TApp (TLambda ts) k
                            --                                   Just (t1, ls) -> foldl (\x (v,t2) -> subs v x t2) t1 ls
                            TLambda ts -> case matchesOnlyDefault (getPats ts) t of
                                                      True -> case ts of
                                                                  [(PVar x, t1)] -> subs x t1 t
                                                                  _ -> case isReduced t of
                                                                            True -> case t of
                                                                                      TVar _ -> TApp (TLambda ts) t -- if it is just a variable it might match a pattern by substituiton.. : TODO: what about an unbound variable?
                                                                                      _ -> case getDefault ts of
                                                                                                Nothing -> TApp (TLambda ts) t
                                                                                                Just (x,t1) -> subs x t1 t
                                                                            False -> TApp (TLambda ts) (reduce t)
                                                      False -> case getMatching ts t of
                                                                    Nothing -> TApp (TLambda ts) (reduce t)
                                                                    Just (t1, ls) -> foldl (\x (v,t2) -> subs v x t2) t1 ls
                            -- TLambda (PVar x) t1 -> subs x t1 t
                            -- TLambda (PSeq x y) t1 -> case t of
                            --                         TSeq t2 t3 -> subs y (subs x t1 t2) t3
                            --                         _ -> TApp (TLambda (PSeq x y) t1) (reduce t)
                            TSub t1 -> TSub (TApp t1 t)
                            TMult t1 t2 -> TMult (TApp t1 t) t2
                            TDiv t1 t2 -> TDiv (TApp t1 t) t2
                            t1@(TSeq _ _) -> applySeqToSeq t1 (reduceMany t) -- force evaluation of t for it's structure
                            t1@(TAlt _ _) -> applyAltToAlt t1 (reduceMany t)
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
reduce (TLambda ts) = TLambda $ mapTerm reduce ts
reduce  t = t

-- _alphaConv :: [Var] -> Term -> Term
-- _alphaConv vs (TLambda (PVar x) t) = case isFree x t && (not $ elem x vs) of
--                                             True -> t
--                                             False -> TLambda (PVar x) (_alphaConv (x:vs) t)
-- _alphaConv vs (TLambda (PSeq x y) t) = case isFree x t && (not $ elem x vs) && isFree y t && (not $ elem y vs) of
--                                             True -> t
--                                             False -> TLambda (PSeq x y) (_alphaConv (x:y:vs) t)
-- _alphaConv vs (TSub t) = TSub $ _alphaConv vs t
-- _alphaConv vs (TSeq t1 t2) = TSeq (_alphaConv vs t1) (_alphaConv vs t2)
-- _alphaConv vs (TAlt t1 t2) = TAlt (_alphaConv vs t1) (_alphaConv vs t2)
-- _alphaConv vs (TStack t1 t2) = TStack (_alphaConv vs t1) (_alphaConv vs t2)
-- _alphaConv vs (TMult t1 t2) = TMult (_alphaConv vs t1) (_alphaConv vs t2)
-- _alphaConv vs (TDiv t1 t2) = TDiv (_alphaConv vs t1) (_alphaConv vs t2)
-- _alphaConv _ t = t

-- alphaConv :: Term -> Term
-- alphaConv = _alphaConv []


reduceMany :: Term -> Term
reduceMany t = case isReduced t of
                    True -> t
                    False -> reduceMany $ reduce t
