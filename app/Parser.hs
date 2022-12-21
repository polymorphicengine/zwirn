module Parser where

import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim

import Language

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

pBool :: TermParser Term
pBool = (symbol "t" >> return (TBool True)) <|> (symbol "f" >> return (TBool False))

pVal :: TermParser Term
pVal = pRest <|> pInt <|> try pBool <|> pVar <|> pParens

pOp1 :: Term -> TermParser Term
pOp1 t = pDiv t <|> pMult t <|> pApp t <|> pStack t

pOp1App :: Term -> TermParser Term
pOp1App t = pDiv t <|> pMult t <|> pStack t

pOp1Seq :: Term -> TermParser Term
pOp1Seq t = pDiv t <|> pMult t <|> pApp t <|> pStack t <|> pSeq t

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
pSeq t = do
  ts <- many1 (symbol " " >> pTerm)
  case ts == [] of
    True -> return $ t
    False -> return $ TSeq t (toTSeq ts)

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
  p <- pPat
  symbol "."
  t <- pTerm
  ts <- many $ do
            symbol "|"
            p1 <- pPat
            symbol "."
            t1 <- pTerm
            return $ (p1,t1)
  return $ TLambda $ (p,t):ts

pPat :: TermParser Pat
pPat = pPatVar

pPatVar :: TermParser Pat
pPatVar = fmap PVar $ many1 letter

pPatSeq :: TermParser Pat
pPatSeq = do
  x <- pPat
  symbol " "
  y <- pPat
  return $ PSeq x y

parseTerm :: String -> Either ParseError Term
parseTerm = runParser (pTermSeq Prelude.<* eof) (0 :: Int) ""
