module Main where

import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim

import Data.Map as Map (Map, fromList, lookup)


type Var = String

data Term = TVar Var
          | TInt Int
          | TRest
          | TSeq Term Term
          | TSub Term
          | TStack Term Term
          | TMult Term Term
          | TDiv Term Term
          | TLambda Var Term
          | TApp Term Term
          | TDes1 Term
          | TDes2 Term
          deriving (Eq, Show)

type Env = Map String Term

envTry :: Env
envTry = fromList [("fast", TLambda "x" (TLambda "y" (TMult (TVar "y") (TVar "x"))))
                  ,("slow", TLambda "x" (TLambda "y" (TDiv (TVar "y") (TVar "x"))))
                  ,("t", TLambda "x" (TLambda "y" (TVar "x")))
                  ,("f", TLambda "x" (TLambda "y" (TVar "y")))
                  ,("if", TLambda "p" (TLambda "a" (TLambda "b" (TApp ( (TApp (TVar "p") (TVar "a"))) (TVar "b")))))
                  ,("while", TLambda "p" (TLambda "h" (TLambda "x" (TApp (TApp (TApp (TVar "if") (TVar "p")) (TApp (TVar "h") (TVar "x"))) (TVar "x")))))
                  ,("Y", (TLambda "g" ( (TApp ( (TLambda "x" (TApp (TVar "g") ( (TApp (TVar "x") (TVar "x")))))) ( (TLambda "z" (TApp (TVar "g") ( (TApp (TVar "z") (TVar "z"))))))))))
                  ,("succ", TLambda "n" (TLambda "g" (TLambda "x" (TApp (TVar "g") ((TApp (TApp (TVar "n") (TVar "g")) (TVar "x")))))))
                  ,("add", TLambda "m" (TLambda "n" (TApp (TApp (TVar "m") (TVar "succ")) (TVar "n"))))
                  ,("pred", TLambda "n" (TLambda "g" (TLambda "x" (TApp (TApp (TApp (TVar "n") (TLambda "j" (TLambda "h" (TApp (TVar "h") (TApp (TVar "j") (TVar "g")))))) (TLambda "u" (TVar "x"))) (TLambda "u" (TVar "u"))))))
                  ,("sub", TLambda "m" (TLambda "n" (TApp ( (TApp (TVar "n") (TVar "pred"))) (TVar "m"))) )
                  ,("iszero", TLambda "n" (TApp ( (TApp (TVar "n") (TLambda "x" (TVar "f")))) (TVar "t")))
                  ,("eq", TLambda "m" (TLambda "n" (TApp (TVar "iszero") ( (TApp ( (TApp (TVar "sub") (TVar "m"))) (TVar "n"))))))
                  ,("loop", TLambda "x" (TApp (TLambda "h" (TSeq (TVar "x") (TApp (TVar "h") (TVar "h")))) (TLambda "h" (TSeq (TVar "x") (TApp (TVar "h") (TVar "h"))))))
                  ,("run", TApp (TVar "Y") (TLambda "h" (TLambda "n" (TApp (TApp (TApp (TVar "if") (TApp (TVar "iszero") (TApp (TVar "pred") (TVar "n")))) (TInt 0)) (TSeq (TApp (TVar "h") (TApp (TVar "pred") (TVar "n"))) (TApp (TVar "pred") (TVar "n")))))))
                  ]

displayTerm :: Term -> String
displayTerm (TVar x) = x
displayTerm (TInt i) = show i
displayTerm (TRest) = "~"
displayTerm (TSeq t1 t2) = displayTerm t1 ++ " " ++ displayTerm t2
displayTerm (TSub t) = "[" ++ displayTerm t ++ "]"
displayTerm (TStack t1 t2) = displayTerm t1 ++ "," ++ displayTerm t2
displayTerm (TMult t1 t2) = displayTerm t1 ++ "*" ++ displayTerm t2
displayTerm (TDiv t1 t2) = displayTerm t1 ++ "/" ++ displayTerm t2
displayTerm (TApp t1 t2) = "(" ++ displayTerm t1 ++ "$" ++ displayTerm t2 ++ ")"
displayTerm (TLambda x t2) = "(\\" ++ x ++ "." ++ displayTerm t2 ++ ")"
displayTerm (TDes1 t) = displayTerm t ++ "#"
displayTerm (TDes2 t) = displayTerm t ++ "##"

type TermParser = Text.Parsec.Prim.Parsec String Int

braces, brackets, parens, angles:: TermParser a -> TermParser a
braces  = between (symbol "{")(symbol "}")
brackets = between (symbol "[")(symbol "]")
parens = between (symbol "(")(symbol ")")
angles = between (symbol "<")(symbol ">")

symbol :: String -> TermParser ()
symbol x = string x >> return ()

pVar :: TermParser Term
pVar = fmap TVar $ many1 letter

pRest :: TermParser Term
pRest = symbol "~" >> return TRest

pInt :: TermParser Term
pInt = fmap TInt $ read <$> many1 digit

pVal :: TermParser Term
pVal = pRest <|> pInt <|> pVar <|> pSub <|> pParens

pOp1 :: Term -> TermParser Term
pOp1 t = pDiv t <|> pMult t <|> pApp t <|> pStack t <|>  pDes2 t <|> pDes1 t

pOp1App :: Term -> TermParser Term
pOp1App t = pDiv t <|> pMult t <|> pStack t <|>  pDes2 t <|> pDes1 t

pOp1Seq :: Term -> TermParser Term
pOp1Seq t = pDiv t <|> pMult t <|> pApp t <|> pSeq t <|> pStack t <|>  pDes2 t <|> pDes1 t

pOp2 :: TermParser Term
pOp2 = pLambda


pTermOnce :: (Term -> TermParser Term) -> TermParser Term
pTermOnce p = (pVal >>= \x -> (p x <|> return x)) <|> pOp2

pTermMany :: (Term -> TermParser Term) -> Term -> TermParser Term
pTermMany p t = (do
  q <- p t
  pTermMany p q) <|> return t

_pTerm :: (Term -> TermParser Term) -> TermParser Term
_pTerm p = (pTermOnce p) >>= (pTermMany p)

pTerm :: TermParser Term
pTerm = _pTerm pOp1

pTermSeq :: TermParser Term
pTermSeq = _pTerm pOp1Seq

pTermApp :: TermParser Term
pTermApp = _pTerm pOp1App


pSeq :: Term -> TermParser Term
pSeq t1 = do
  symbol " "
  t2 <- pTerm
  return $ TSeq t1 t2

pDiv :: Term -> TermParser Term
pDiv t1 = do
  symbol "/"
  t2 <- pTerm
  return $ TDiv t1 t2

pMult :: Term -> TermParser Term
pMult t1 = do
  symbol "*"
  t2 <- pTerm
  return $ TMult t1 t2

pStack :: Term -> TermParser Term
pStack t1 = do
  symbol ","
  t2 <- pTerm
  return $ TStack t1 t2

pSub :: TermParser Term
pSub = fmap TSub $ brackets $ pTermSeq

pParens :: TermParser Term
pParens = parens pTermSeq

pApp :: Term -> TermParser Term
pApp t1 = do
  symbol "$"
  t2 <- pTermApp
  return $ TApp t1 t2

pLambda :: TermParser Term
pLambda = do
  _ <- char '\\'
  x <- many1 letter
  symbol "."
  t <- pTerm
  return $ TLambda x t

pDes1 :: Term -> TermParser Term
pDes1 t = symbol "#" >> (return $ TDes1 t)

pDes2 :: Term -> TermParser Term
pDes2 t = symbol "§" >> (return $ TDes2 t)

parseTerm :: String -> Either ParseError Term
parseTerm = runParser (pTermSeq Prelude.<* eof) (0 :: Int) ""


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
fromChurch (TStack t1 t2) = TStack (fromChurch t1) (fromChurch t2)
fromChurch (TMult t1 t2) = TMult (fromChurch t1) (fromChurch t2)
fromChurch (TDiv t1 t2) = TDiv (fromChurch t1) (fromChurch t2)
fromChurch (TApp t1 t2) = TApp (fromChurch t1) (fromChurch t2)
fromChurch (TSub t) = TSub (fromChurch t)
fromChurch (TLambda x t) = TLambda x (fromChurch t)
fromChurch (TDes1 t) = TDes1 (fromChurch t)
fromChurch (TDes2 t) = TDes2 (fromChurch t)
fromChurch x = x

nAppRev :: Term -> Maybe Int
nAppRev (TVar "x") = Just 0
nAppRev (TApp (TVar "g") t) = fmap (+1) (nAppRev t)
nAppRev _ = Nothing

destruct1 :: Term -> Term
destruct1 (TSeq t1 _) = t1
destruct1 (TSub t) = destruct1 t
destruct1 (TStack t1 _) = t1
destruct1 (TMult t1 _) = t1
destruct1 (TDiv t1 _) = t1
destruct1 (TApp t1 _) = t1
destruct1 (TLambda x _) = TVar x
destruct1 (TDes1 t1) = t1
destruct1 (TDes2 t1) = t1
destruct1 t = t

destruct2 :: Term -> Term
destruct2 (TSeq _ t2) = t2
destruct2 (TSub t) = destruct2 t
destruct2 (TStack _ t2) = t2
destruct2 (TMult _ t2) = t2
destruct2 (TDiv _ t2) = t2
destruct2 (TApp _ t2) = t2
destruct2 (TLambda _ t2) = t2
destruct2 (TDes1 t1) = t1
destruct2 (TDes2 t1) = t1
destruct2 t = t

subs :: Var -> Term -> Term -> Term
subs x (TVar y) t = if x == y then t else (TVar y)
subs _ t@(TInt _) _ = t
subs _ TRest _ = TRest
subs x (TSeq t1 t2) t = TSeq (subs x t1 t) (subs x t2 t)
subs x (TStack t1 t2) t = TStack (subs x t1 t) (subs x t2 t)
subs x (TMult t1 t2) t = TMult (subs x t1 t) (subs x t2 t)
subs x (TDiv t1 t2) t = TDiv (subs x t1 t) (subs x t2 t)
subs x (TApp t1 t2) t = TApp (subs x t1 t) (subs x t2 t)
subs x (TSub t1) t = TSub (subs x t1 t)
subs x (TLambda y t1) t = if x /= y then TLambda y (subs x t1 t) else TLambda y t1
subs x (TDes1 t1) t = TDes1 (subs x t1 t)
subs x (TDes2 t1) t = TDes2 (subs x t1 t)


-- in the case of multiplication or division the application is only propagated to the value of the term, not the modifiers(tail fs)

isFree :: Var -> Term -> Bool
isFree x (TVar y) = x /= y
isFree x (TSub t) = isFree x t
isFree x (TDes1 t) = isFree x t
isFree x (TDes2 t) = isFree x t
isFree x (TSeq t1 t2) = (isFree x t1) && (isFree x t2)
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
isReduced (TSub t) = isReduced t
isReduced (TSeq t1 t2) = isReduced t1 && isReduced t2
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
isReduced (TDes1 _) = False
isReduced (TDes2 _) = False

reduce :: Term -> Term
reduce (TApp f t) = case reduce f of
                            TLambda x t1 -> subs x t1 t
                            TSub t1 -> TSub (TApp t1 t)
                            TMult t1 t2 -> TMult (TApp t1 t) t2
                            TDiv t1 t2 -> TDiv (TApp t1 t) t2
                            t1@(TSeq _ _) -> applySeqToSeq t1 t
                            t2 -> TApp t2 (reduce t)
reduce (TSub t) = TSub (reduce t)
reduce (TVar x) = case Map.lookup x envTry of
                              Just t -> t
                              Nothing -> TVar x
reduce (TInt i) = toChurch i
reduce (TSeq t1 t2) = TSeq (reduce t1) (reduce t2)
reduce (TMult t1 t2) = TMult (reduce t1) (reduce t2)
reduce (TDiv t1 t2) = TDiv (reduce t1) (reduce t2)
reduce (TDes1 t) = destruct1 t
reduce (TDes2 t) = destruct2 t
reduce (TLambda x t) = TLambda x (reduce t)
reduce  t = t

_alphaConv :: [Var] -> Term -> Term
_alphaConv vs (TLambda x t) = case isFree x t && (not $ elem x vs) of
                                            True -> t
                                            False -> TLambda x (_alphaConv (x:vs) t)
_alphaConv vs (TSub t) = TSub $ _alphaConv vs t
_alphaConv vs (TDes1 t) = TDes1 $ _alphaConv vs t
_alphaConv vs (TDes2 t) = TDes2 $ _alphaConv vs t
_alphaConv vs (TSeq t1 t2) = TSeq (_alphaConv vs t1) (_alphaConv vs t2)
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


tSeqLen :: Term -> Int
tSeqLen (TSeq t1 _) = 1 + tSeqLen t1
tSeqLen _ = 1

getTSeq :: Term -> [Term]
getTSeq (TSeq t1 t2) = getTSeq t1 ++ [t2]
getTSeq t = [t]


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

toTSeq :: [Term] -> Term
toTSeq = _toTSeq . reverse

_toTSeq :: [Term] -> Term
_toTSeq [] = TRest
_toTSeq [x] = x
_toTSeq (t:ts) = TSeq (_toTSeq $ ts) t

run :: Term -> String
run = displayTerm . alphaConv . fromChurch . reduceMany

main :: IO ()
main = do
  putStrLn $ "Please enter a mini lambda program:"
  l <- readLn
  case parseTerm l of
    Left err -> putStrLn $ show err
    Right t -> do
        putStrLn "Parsing the term succeeded!"
        putStrLn "Proceeding to run it.."
        putStrLn $ "The result:"
        putStrLn $ run t
        putStrLn "Proceeding to the next program.."
        main
