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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

pInteger :: Parser Integer
pInteger =  lexeme L.decimal

pFloat :: RealFloat a => Parser a
pFloat = lexeme L.float

pString :: Parser String
pString = lexeme $ (:) <$> letterChar <*> many alphaNumChar

-- parsing simple values

pVar :: Parser Term
pVar = do
  x <- pString
  return $ TVar x

pQuote :: Parser Term
pQuote = do
  _ <- symbol "\""
  x <- pString
  _ <- symbol "\""
  return $ TVar ("\"" ++ x ++ "\"")

pRest :: Parser Term
pRest = symbol "~" >> return TRest

pNum :: Parser Term
pNum = try (fmap (TVar . show) pFloat) <|> (fmap (TVar . show) pInteger)


pVal :: Parser Term
pVal = pRest <|> pNum <|> pVar <|> pQuote

-- parsing of a sequence of terms is context depended, usually it is parsed as
-- function application, within brackets [] it is parsed as a tidal sequence
-- * and / associate to the left

topOps :: [[Operator Parser Term]]
topOps = [[manyPostfix "@" TElong], [binaryL "%" TPoly], [binaryL "//" (TOp "//")], [ binaryL  "*"  TMult, binaryL  "/"  TDiv]]

topParser :: Parser Term
topParser = makeExprParser (pVal <|> pChoiceSeq <|> pAltExp <|> parens fullParser) topOps


pSeqExp :: Parser Term
pSeqExp = do
      expr <- topParser
      pSeq expr <|> return expr
      where pSeq t = do
                ts <- some topParser
                return $ TSeq (t:ts)

pStackSeq :: Parser Term
pStackSeq = do
     t <- pSeqExp
     pStack t <|> return t
     where pStack t = do
              ts <- some $ (symbol "," >> pSeqExp)
              return $ TStack (t:ts)

pChoiceSeq :: Parser Term
pChoiceSeq = brackets $ do
     t <- pStackSeq
     pChoice t <|> return t
     where pChoice t = do
              ts <- some $ (symbol "|" >> pStackSeq)
              return $ TChoice (t:ts)

pAltExp :: Parser Term
pAltExp = angles $ do
      expr <- topParser
      pSeq expr <|> return expr
      where pSeq t = do
                ts <- some topParser
                return $ TAlt (t:ts)


bottomOps :: [[Operator Parser Term]]
bottomOps = [[ binaryL  ""  TApp ]
            ,[binaryR "|*|" (TOp "|*|")
             ,binaryR "|*" (TOp "|*")
             ,binaryR "|+" (TOp "|+")
             ,binaryR "+|" (TOp "+|")
             ,binaryR "+" (TOp "+")
             ,binaryR "|-" (TOp "|-")
             ,binaryR "-|" (TOp "-|")
             ,binaryR "-" (TOp "-")
             -- ,binaryR "*|" (TOp "*|") syntactically not possible
             ]
            ,[binaryL "#" (TOp "#")], [binaryR "$" (TOp "$$")]
            ]

bottomParser :: Parser Term
bottomParser = makeExprParser (topParser <|> pLambda) bottomOps


pStackApp :: Parser Term
pStackApp = do
     t <- bottomParser
     pStack t <|> return t
     where pStack t = do
              ts <- some $ (symbol "," >> bottomParser)
              return $ TStack (t:ts)

-- choice binds weakest
pChoiceApp :: Parser Term
pChoiceApp = do
  t <- pStackApp
  pChoice t <|> return t
  where pChoice t = do
           ts <- some $ (symbol "|" >> pStackApp)
           return $ TChoice (t:ts)

fullParser :: Parser Term
fullParser = pChoiceApp

binaryR :: String -> (Term -> Term -> Term) -> Operator Parser Term
binaryR name f = InfixR (f <$ (symbol name))

binaryL :: String -> (Term -> Term -> Term) -> Operator Parser Term
binaryL name f = InfixL (f <$ (symbol name))

postfix :: String -> (Term -> Term) -> Operator Parser Term
postfix name f = Postfix (f <$ symbol name)

manyPostfix :: String -> (Term -> Term) -> Operator Parser Term
manyPostfix name f = Postfix $ foldr1 (.) <$> some (f <$ symbol name)

pLambda :: Parser Term
pLambda = do
  _ <- symbol "\\"
  x <- pString
  _ <- symbol "->"
  t <- fullParser
  return $ TLambda x t

parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm s = runParser (fullParser <* eof) "" s
