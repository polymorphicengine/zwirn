{
module Zwirn.Language.Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  , scanMany
  , increaseChoice
  , setEditorNum
  , getEditorNum
  , setInitialLineNum
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (when)
}

%wrapper "monadUserState-strict-text"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ )*
@op = (( "|" | "+" | "*" | "/" | "&" | "=" | "~" | "$" | "%" | "?" | "." | "-" | "#" | "^") ( "<" | ">" )?)+
@num = ("-")? ($digit)+ ("." ($digit)+)?

tokens :-

<0> $white+ ;

-- Multi Line Comments

<0>       "{-" { nestComment `andBegin` comment }
<0>       "-}" { \_ _ -> alexError "Error: unexpected closing comment" }
<comment> "{-" { nestComment }
<comment> "-}" { unnestComment }
<comment> .    ;
<comment> \n   ;

-- Single Line Comments

<0> "--" .* ;

-- Repeat
<0> "!"     { tok Repeat }

-- Elongate
<0> "@"     { tok Elongate }

-- Parenthesis
<0> "("     { tok LPar }
<0> ")"     { tok RPar }

-- Sequences
<0> "["     { tok LBrack }
<0> "]"     { tok RBrack }

-- Stacks
<0> ","     { tok Comma }

-- Choice
<0> "|"     { tok Pipe }

-- Polyrhythm
<0> "%"     { tok Poly }

-- Euclid
<0> "{"     { tok LBraces }
<0> "}"     { tok RBraces }

-- Lambda
<0> "\"     { tok Lambda }
<0> "->"    { tok Arrow }

-- Actions
<0> ";"                               { tok Colon }
<0> "<-"                              { tok StreamA }
<0> ":t"                              { tok TypeA }
<0> "="                               { tok Assign }
<0> (":load") $white+ ($alpha | "/")+ { tokText (\t -> LoadA $ Text.drop 5 t) }
<0> ":js"                             { tok JSA }

-- Identifiers
<0> @id     { tokText Identifier }

-- Constants
<0> @num            { tokText Number }
<0> \"[^\"]*\"      { tokText String }
<0> "~"             { tok Rest }

-- Operators
<0> @op             { tokText Operator }

-- Alternations
<0> "<"     { tok LAngle }
<0> ">"     { tok RAngle }

{
data AlexUserState = AlexUserState
  { nestLevel :: Int
  , choiceNum :: Int
  , editorNum :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { nestLevel = 0, choiceNum = 0, editorNum = 0}

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  -- Identifiers
  = Identifier Text
  -- Constants
  | String Text
  | Number Text
  | Rest
  -- Operators
  | Operator Text
  -- Repeat
  | Repeat
  -- Elongation
  | Elongate
  -- Parenthesis
  | LPar
  | RPar
  -- Sequences
  | LBrack
  | RBrack
  -- Stacks
  | Comma
  -- Alternations
  | LAngle
  | RAngle
  -- Choice
  | Pipe
  -- Polyrhythm
  | Poly
  -- Euclid
  | LBraces
  | RBraces
  -- Lambda
  | Lambda
  | Arrow
  -- Actions
  | Colon
  | StreamA
  | TypeA
  | Assign
  | LoadA Text
  | JSA
  -- EOF
  | EOF
  deriving (Eq)

instance Show Token where
 show (Identifier s) = show s
 show (String s) = show s
 show (Number d) = show d
 show Rest = quoted "~"
 show (Operator o) = show o
 show Repeat = quoted "!"
 show Elongate = quoted "@"
 show LPar = quoted "("
 show RPar = quoted ")"
 show LBrack = quoted "["
 show RBrack = quoted "]"
 show Comma = quoted ","
 show LAngle = quoted "<"
 show RAngle = quoted ">"
 show Pipe = quoted "|"
 show Poly = quoted "%"
 show LBraces = quoted "{"
 show RBraces = quoted "}"
 show Lambda = quoted "\\"
 show Arrow = quoted "->"
 show Colon = quoted ";"
 show StreamA = quoted "<-"
 show TypeA = quoted ":t"
 show Assign = quoted "="
 show (LoadA x) = ":load " <> show x
 show JSA = quoted ":js"
 show EOF = "end of file"

quoted :: String -> String
quoted s = "'" ++ s ++ "'"

mkRange :: AlexInput -> Int -> Range
mkRange (st, _, _, str) len = Range{start = st, stop = end}
  where
    end = Text.foldl' alexMove st $ Text.take len str

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokText :: (Text -> Token) -> AlexAction RangedToken
tokText f inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = f $ Text.take len str
    , rtRange = mkRange inp len
    }

nestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction RangedToken
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

increaseChoice :: Alex Int
increaseChoice = do
  (AlexUserState c x e) <- get
  put $ AlexUserState c (x+1) e
  return x

getEditorNum :: Alex Int
getEditorNum = do
  (AlexUserState _ _ e) <- get
  return e

setEditorNum :: Int -> Alex ()
setEditorNum i = do
  (AlexUserState c x _) <- get
  put $ AlexUserState c x i

setInitialLineNum :: Int -> Alex ()
setInitialLineNum i = Alex alex
                    where alex s = Right (s {alex_pos = AlexPn x i c }, ())
                                 where AlexPn x _ c = alex_pos s


scanMany :: Text -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [rtToken output]
        else ((rtToken output) :) <$> go
}