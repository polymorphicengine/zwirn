module Megaparsec where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Sound.Tidal.ID (ID(..))
import qualified Control.Monad.State as ST (evalState, State, modify, get)
import qualified Text.Megaparsec.Char.Lexer as L

import Language

type Parser = ParsecT Void String (ST.State (Int,Int))

operators :: [String]
operators = ["//"
            ,"*|"
            ,"|*|"
            ,"|*"
            ,"|+"
            ,"+|"
            ,"+"
            ,"|-"
            ,"-|"
            ,"-"
            ,"~>"
            ,"<~"
            ,">="
            ,"<="
            ,"=="
            ,"&&"
            ,"||"
            ,"?"
            ]

-- lexer and helpers

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

asum :: [Parser a] -> Parser a
asum = foldr (<|>) empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbolNoSpace :: String -> Parser String
symbolNoSpace = L.symbol (return ())

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

pInteger :: Parser Integer
pInteger = L.decimal

pFloat :: RealFloat a => Parser a
pFloat = L.float

pOp :: Parser String
pOp = asum $ map symbolNoSpace operators

pString :: Parser String
pString = ((:) <$> letterChar <*> many alphaNumChar) <|> pOp

-- parsing simple values
-- note that we don't store the end line number of a value, we assume every value
-- starts ad ends on the same line, this allows us to store the number of the editor window
-- in which the value is in there, so we can highlight across multiple editor windows

getCoord :: Parser (Int,Int)
getCoord = do
  pos <- getSourcePos
  return (unPos $ sourceLine pos,unPos $ sourceColumn pos)

getCoord2 :: Parser (Int,Int)
getCoord2 = do
  pos <- getSourcePos
  editorNum <- fmap snd ST.get
  return (editorNum, unPos $ sourceColumn pos)

pVar :: Parser Term
pVar = lexeme $ (do
  i <- getCoord
  x <- pString
  j <- getCoord2
  return $ TVar (i,j) x
  <?> "variable")

pQuote :: Parser Term
pQuote = lexeme $ (do
  i <- getCoord
  _ <- symbol "\""
  x <- pString
  _ <- symbolNoSpace "\""
  j <- getCoord2
  return $ TVar (i,j) ("\"" ++ x ++ "\"")
  <?> "string")

pRest :: Parser Term
pRest = symbol "~" >> (return TRest) <?> "rest"

pNum :: Parser Term
pNum = lexeme $ (do
  i <- getCoord
  n <- try (fmap (show :: Double -> String) pFloat) <|> (fmap show pInteger)
  j <- getCoord2
  return $ TVar (i,j) n
  <?> "number")


pVal :: Parser Term
pVal = pRest <|> pNum <|> pVar <|> pQuote


-- parsing of a sequence of terms is context depended, usually it is parsed as
-- function application, within brackets [] it is parsed as a tidal sequence
-- * and / associate to the left

-- should they all be binaryL ?
parserOps :: [Operator Parser Term]
parserOps = map (\x -> binaryL x (TOp x)) operators


topOps :: [[Operator Parser Term]]
topOps = [[manyPostfix "@" TElong], [pEuclidPost], [binaryL "%" TPoly], parserOps, [ binaryL  "*"  TMult, binaryL  "/"  TDiv]]

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
              seed <- fmap fst ST.get
              ST.modify $ (\(x,y) -> (x+1,y))
              return $ TChoice seed (t:ts)

pAltExp :: Parser Term
pAltExp = angles $ do
      expr <- topParser
      pSeq expr <|> return expr
      where pSeq t = do
                ts <- some topParser
                return $ TAlt (t:ts)


bottomOps :: [[Operator Parser Term]]
bottomOps = [[ binaryL  ""  TApp ]
             ,[binaryR "." (TOp ".")]
             ,[binaryL "#" (TOp "#")]
             ,[binaryR "$" (TOp "$")]
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
           seed <- fmap fst ST.get
           ST.modify $ (\(x,y) -> (x+1,y))
           return $ TChoice seed (t:ts)

fullParser :: Parser Term
fullParser = pChoiceApp

binaryR :: String -> (Term -> Term -> Term) -> Operator Parser Term
binaryR name f = InfixR (f <$ (symbol name))

binaryL :: String -> (Term -> Term -> Term) -> Operator Parser Term
binaryL name f = InfixL (f <$ (symbol name))

postfix :: String -> (Term -> Term) -> Operator Parser Term
postfix name f = Postfix (f <$ symbol name)

prefix :: String -> (Term -> Term) -> Operator Parser Term
prefix name f = Prefix (f <$ symbol name)

manyPostfix :: String -> (Term -> Term) -> Operator Parser Term
manyPostfix name f = Postfix $ foldr1 (.) <$> some (f <$ symbol name)

pEuclidPost :: Operator Parser Term
pEuclidPost = Postfix $ do
          _ <- symbol "{"
          n <- fullParser
          _ <- symbol ";"
          m <- fullParser
          mayK <- (Just <$> (symbol ";" >> fullParser)) <|> pure Nothing
          _ <- symbol "}"
          case mayK of
            Just k -> return $ (\x -> TEuclid x n m k)
            Nothing -> return $ (\x -> TEuclid x n m (TVar ((0,0),(0,0)) "0")) --position is wrong

pLambda :: Parser Term
pLambda = do
  _ <- symbol "\\"
  xs <- some $ lexeme pString
  _ <- symbol "->"
  t <- fullParser
  return $ TLambda xs t

-- parsing definitions

parserDef :: Parser Def
parserDef = do
      --_ <- symbol "let"
      name <- lexeme pString
      vs <- many $ lexeme pString
      _ <- symbol "="
      t <- fullParser
      return $ Let name vs t

-- parsing execs

parserID :: Parser ID
parserID = (do
      _ <- symbol "\""
      n <- lexeme pString
      _ <- symbol "\""
      return $ ID n)
      <|> fmap (ID . show) (lexeme pInteger)

parserExec :: Parser Action
parserExec = do
         idd <- parserID
         _ <- symbol "<-"
         t <- fullParser
         return $ Exec idd t

-- parsing actions

parserTypes :: Parser Action
parserTypes = do
          _ <- symbol ":t"
          t <- fullParser
          return $ Type t

parserShow :: Parser Action
parserShow = do
          _ <- symbol ":show"
          t <- fullParser
          return $ Show t

parserAction :: Parser Action
parserAction = sc >> (try parserShow <|> parserTypes <|> try (fmap Def parserDef) <|> parserExec)

-- initial state

initialState :: SourcePos -> String -> State String Void
initialState pos input = State
      { stateInput = input
      , stateOffset = 0
      , statePosState = PosState
        { pstateInput = input
        , pstateOffset = 0
        , pstateSourcePos = pos
        , pstateTabWidth = mkPos 4
        , pstateLinePrefix = ""
        }
      , stateParseErrors = []
      }

parseWithPos :: Int -> Int -> String -> Either (ParseErrorBundle String Void) Action
parseWithPos editorNum line s = snd (ST.evalState (runParserT' (parserAction <* eof) (initialState pos s)) (0,editorNum))
                    where pos = SourcePos "" (mkPos line) (mkPos 1)
