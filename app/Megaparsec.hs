module Megaparsec where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Language

type Parser = Parsec Void String

-- lexer and helpers

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pInteger :: Parser Integer
pInteger =  lexeme L.decimal

pString :: Parser String
pString = lexeme $ (:) <$> letterChar <*> many alphaNumChar

-- parsing simple values

pVar :: Parser Term
pVar = do
  xs <- pString
  case xs of
    "t" -> fail "reserved"
    "f" -> fail "reserved"
    _ -> return $ TVar xs

pRest :: Parser Term
pRest = symbol "~" >> return TRest

pInt :: Parser Term
pInt = fmap TInt (fmap fromIntegral pInteger)

pBool :: Parser Term
pBool = (symbol "t" >> return (TBool True)) <|> (symbol "f" >> return (TBool False))

pVal :: Parser Term
pVal = pRest <|> pInt <|> try pVar <|> pBool

-- seqeuences bind less than * and /,
-- but more than , which binds more than $
-- * / and $ associate to the left
-- seqeuences and stacks to the right

topOps :: [[Operator Parser Term]]
topOps = [[ binaryL  "*"  TMult, binaryL  "/"  TDiv]]

topParser :: Parser Term
topParser = makeExprParser (pVal <|> parens bottomParser) topOps


pSeqExp :: Parser Term
pSeqExp = do
      expr <- topParser
      pSeq expr <|> return expr
      where pSeq t = do
                ts <- some topParser
                return $ toTSeq (t:ts)


bottomOps :: [[Operator Parser Term]]
bottomOps = [[ binaryR "," TStack ],[ binaryL  "$"  TApp ]]

bottomParser :: Parser Term
bottomParser = makeExprParser (pSeqExp <|> pLambda <|> parens bottomParser) bottomOps



binaryR :: String -> (Term -> Term -> Term) -> Operator Parser Term
binaryR name f = InfixR (f <$ symbol name)

binaryL :: String -> (Term -> Term -> Term) -> Operator Parser Term
binaryL name f = InfixL (f <$ symbol name)

-- parsing patterns

pPat :: Parser Pat
pPat = try pPatSeq <|> try pPatBool <|> pPatVar <|> pPatInt

pPatVar :: Parser Pat
pPatVar = fmap PVar $ pString

pPatInt :: Parser Pat
pPatInt = fmap PInt $ (fmap fromIntegral pInteger)

pPatBool :: Parser Pat
pPatBool = (symbol "t" >> (return $ PBool True)) <|> (symbol "f" >> (return $ PBool False))

pPatSeq :: Parser Pat
pPatSeq = do
  x <- pPatVar <|> parens pPat
  y <- pPatVar <|> parens pPat
  return $ PSeq x y


pLambda :: Parser Term
pLambda = do
  _ <- symbol "\\"
  p <- pPat
  _ <- symbol "->"
  t <- bottomParser
  ts <- many $ do
            _ <- symbol "|"
            p1 <- pPat
            _ <- symbol "->"
            t1 <- bottomParser
            return $ (p1,t1)
  return $ TLambda $ (p,t):ts

parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm s = runParser (bottomParser <* eof) "" s
