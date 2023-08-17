{
{-# LANGUAGE OverloadedStrings #-}
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
  , lineLexer
  , typeLexer
  ) where

{-
    Lexer.hs - lexer for zwirn, code adapted from
    https://serokell.io/blog/lexing-with-alex
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import           Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (when)
}

%wrapper "monadUserState-strict-text"

$digit = [0-9]
$alphasmall = [a-z]
$alpha = [a-zA-Z]

@id = ($alphasmall) ($alpha | $digit | \_ )*
@singles = ("+" | "&" | "$" | "?" | "-" | "#" | "." | "^")
@otherops = ("|" | "=" | "~" | "<" | ">" | "%")
@specialop = ("*" | "/" | "'")
@op = ((@singles (@singles | @otherops | @specialop)*) | ((@otherops | @specialop) (@singles | @otherops | @specialop)*))
@num = ("-")? ($digit)+ ("." ($digit)+)?

tokens :-

<0> $white+ ;

<line> (.+ (\n?) | \n)               { mkLine }

<ty> $white+ ;
<ty> $alpha+ "." ;
<ty> "::"                          { tok DoubleColon }
<ty> "P"                           { tok PTypeFam }
<ty> "=>"                          { tok Context }
<ty> "->"                          { tok Arrow }
<ty> "("                           { tok LPar }
<ty> ")"                           { tok RPar }
<ty> ","                           { tok Comma }
<ty> "TextPattern"                 { tok TextToken }
<ty> "NumberPattern"               { tok NumberToken }
<ty> "ControlPattern"              { tok ControlToken }
<ty> "Pattern " [a-z]              { tokText (\t -> VarToken $ Text.drop 8 t) }
<ty> $alpha+ " " [a-z]             { tokText (\t -> TypeClass (Text.take (Text.length t - 2) t) (Text.pack [Text.last t])) }
<ty> @id                           { tokText Identifier }
<ty> @op                           { tokText Operator }
<ty> @specialop                    { tokText SpecialOp }

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
<0> ":cps"                            { tok TempoCps }
<0> ":bpm"                            { tok TempoBpm }
<0> ":t"                              { tok TypeA }
<0> "="                               { tok Assign }
<0> ":show"                           { tok ShowA }
<0> ":config"                         { tok ConfigA }
<0> ":resetconfig"                    { tok ResetConfigA }
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
<0> @specialop      { tokText SpecialOp }

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
  | SpecialOp Text
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
  | TempoCps
  | TempoBpm
  | TypeA
  | ShowA
  | ConfigA
  | ResetConfigA
  | Assign
  | LoadA Text
  | JSA
  -- Line & Block Tokens
  | LineT Text
  | BlockSep
  -- Type Tokens
  | DoubleColon
  | PTypeFam
  | Context
  | TextToken
  | NumberToken
  | ControlToken
  | VarToken Text
  | TypeClass Text Text
  -- EOF
  | EOF
  deriving (Eq)

instance Show Token where
 show (Identifier s) = show s
 show (String s) = show s
 show (Number d) = show d
 show Rest = quoted "~"
 show (Operator o) = show o
 show (SpecialOp o) = show o
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
 show TempoCps = ":cps"
 show TempoBpm = ":bpm"
 show TypeA = quoted ":t"
 show ShowA = quoted ":show"
 show ConfigA = quoted ":config"
 show ResetConfigA = quoted ":resetconfig"
 show Assign = quoted "="
 show (LoadA x) = ":load " <> show x
 show JSA = quoted ":js"
 show (LineT t) = "line " <> show t
 show BlockSep = "block"
 show DoubleColon = "::"
 show PTypeFam = "P"
 show Context = "=>"
 show TextToken = "Text"
 show NumberToken = "Number"
 show ControlToken = "ValueMap"
 show (VarToken t) = show t
 show (TypeClass c v) = show c ++ " " ++ show v
 show EOF = "end of file"

quoted :: String -> String
quoted s = "'" ++ s ++ "'"

mkRange :: AlexInput -> Int -> Range
mkRange (st, _, _, str) len = Range{start = st, stop = end}
  where
    end = Text.foldl' alexMove st $ Text.take len str

mkLine :: AlexAction RangedToken
mkLine inp@(_, _, _, str) len = case Text.all (\c -> elem c ("\n\t " :: String)) (Text.take len str) of
                            True -> tok BlockSep inp len
                            False -> pure RangedToken
                              { rtToken = LineT $ Text.map replaceTab (Text.take len str)
                              , rtRange = mkRange inp len
                              }

-- | replace all tabs with a single space, since codemirror sees tabs as one column
replaceTab :: Char  -> Char
replaceTab '\t' = ' '
replaceTab x = x

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

lineLexer :: Alex ()
lineLexer = alexSetStartCode line

typeLexer :: Alex ()
typeLexer = alexSetStartCode ty

scanMany :: Text -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- lineLexer >> alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else ((output) :) <$> go
}
