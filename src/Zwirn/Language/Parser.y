{
{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Parser
    ( parseActionsWithPos
    , parseActions
    , parseBlocks
    , parseScheme
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.List (intercalate, sortOn)

import qualified Zwirn.Language.Lexer as L
import Zwirn.Language.Syntax
import Zwirn.Language.TypeCheck.Types
import Zwirn.Language.TypeCheck.Infer
import Zwirn.Language.Block

{-
    Parser.hs - parser for zwirn, code adapted from
    https://serokell.io/blog/parsing-with-happy
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


}

%name parse term
%name pActions actions
%name pBlocks blocks
%name pScheme scheme
%tokentype { L.RangedToken }
%errorhandlertype explist
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }
%expect 0

%token
  -- Identifiers
  identifier      { L.RangedToken (L.Identifier _) _ }
  -- Operators
  operator        { L.RangedToken (L.Operator _) _ }
  specop          { L.RangedToken (L.SpecialOp _) _ }
  -- Constants
  string          { L.RangedToken (L.String _) _ }
  number          { L.RangedToken (L.Number _) _ }
  line            { L.RangedToken (L.LineT _) _ }
  bsep            { L.RangedToken (L.BlockSep) _ }
  '~'             { L.RangedToken L.Rest _ }
  -- Repeat
  '!'             { L.RangedToken L.Repeat _ }
  repnum          { L.RangedToken (L.RepeatNum _) _}
  -- Parenthesis
  '('             { L.RangedToken L.LPar _ }
  ')'             { L.RangedToken L.RPar _ }
  -- Sequences
  '['             { L.RangedToken L.LBrack _ }
  ']'             { L.RangedToken L.RBrack _ }
  -- Stacks
  ','             { L.RangedToken L.Comma _ }
  -- Alternations
  '<'             { L.RangedToken L.LAngle _ }
  '>'             { L.RangedToken L.RAngle _ }
  -- Choice
  '|'             { L.RangedToken L.Pipe _ }
  -- Enum
  '..'            { L.RangedToken L.Enum _ }
  -- Polyrhythm
  '%'             { L.RangedToken L.Poly _ }
  -- Lambda
  '\\'            { L.RangedToken L.Lambda _ }
  '->'            { L.RangedToken L.Arrow _ }
  -- Actions
  ';'             { L.RangedToken L.Colon _ }
  '<-'            { L.RangedToken L.StreamA _ }
  ':cps'          { L.RangedToken L.TempoCps _ }
  ':bpm'          { L.RangedToken L.TempoBpm _ }
  ':t'            { L.RangedToken L.TypeA _ }
  ':show'         { L.RangedToken L.ShowA _ }
  ':config'       { L.RangedToken L.ConfigA _ }
  ':resetconfig'  { L.RangedToken L.ResetConfigA _ }
  '='             { L.RangedToken L.Assign _ }
  ':load'         { L.RangedToken (L.LoadA _ ) _}
  ':info'           { L.RangedToken L.InfoA _ }
  -- Type Tokens
  '=>'            { L.RangedToken L.Context _ }
  textT           { L.RangedToken L.TextToken _ }
  numT            { L.RangedToken L.NumberToken _ }
  mapT            { L.RangedToken L.MapToken _ }
  busT            { L.RangedToken L.BusToken _ }
  varT            { L.RangedToken (L.VarToken _) _ }
  classT          { L.RangedToken (L.TypeClass _) _ }

%%

-------------------------------------------------------------
------------------------- utilities -------------------------
-------------------------------------------------------------

optional(p)
  :                                             { Nothing }
  | p                                           { Just $1 }

many_rev(p)
  :                                             { [] }
  | many_rev(p) p                               { $2 : $1 }

many(p)
  : many_rev(p)                                 { reverse $1 }

some_rev(p)
  : p                                           { [$1] }
  | some_rev(p) p                               { $2 : $1 }

some(p)
  : some_rev(p)                                 { reverse $1 }

sepBy_rev(p, sep)
  : p                                           { [$1] }
  | sepBy_rev(p, sep) sep p                     { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep)                           { reverse $1 }

sepBy_rev2(p, sep)
  : p sep p                                     { [$3, $1] }
  | sepBy_rev2(p, sep) sep p                    { $3 : $1 }

sepBy2(p, sep)
  : sepBy_rev2(p, sep)                          { reverse $1 }

-------------------------------------------------------------
----------------------- parsing terms -----------------------
-------------------------------------------------------------

atom :: { Term }
  : identifier                                  { % (mkAtom TVar) $1 }
  | number                                      { % (mkAtom TNum) $1 }
  | string                                      { % (mkAtom TText) $1 }
  | '~'                                         { TRest }

simpleseq :: { [Term] }
  : infix                                %shift { [$1] }
  | infix simpleseq                             { $1: $2 }

seq :: { Term }
  : simpleseq                                   { TSeq $1 }
  | infix '..' infix                            { TEnum Run $1 $3 }
  | infix infix '..' infix                      { TEnumThen Run $1 $2 $4 }
  |                                             { TRest }

sequence :: { Term }
  :  '[' seq ']'                                { $2 }

choice :: { Term }
  : '[' sepBy2(simpleseq, '|') ']'                             { % L.increaseChoice >>= \x -> return $ TChoice x (map TSeq $2) }
  | '[' simpleseq '|' '..' simpleseq ']'                       { TEnum Choice (TSeq $2) (TSeq $5) }
  | '[' simpleseq '|' simpleseq '..' simpleseq ']'             { TEnumThen Choice (TSeq $2) (TSeq $4) (TSeq $6) }

lambda :: { Term }
  : '\\' some(identifier) '->' term      %shift { TLambda (map unTok $2) $4 }

polyrhythm :: { Term }
  : simple '%' simple                    %shift { TPoly $1 $3 }

repeat :: { Term }
  : simple repnum                               { TRepeat $1 (Just $ read $ Text.unpack $ unTok $2) }
  | simple '!'                                  { TRepeat $1 Nothing }

stack :: { Term }
  : '[' sepBy2(simpleseq, ',') ']'                   { TStack (map TSeq $2) }
  | '[' simpleseq ',' '..' simpleseq ']'             { TEnum Cord (TSeq $2) (TSeq $5) }
  | '[' simpleseq ',' simpleseq '..' simpleseq ']'   { TEnumThen Cord (TSeq $2) (TSeq $4) (TSeq $6) }

alt :: { Term }
  : simpleseq                                   { TAlt $1 }
  | infix '..' infix                            { TEnum Alt $1 $3 }
  | infix infix '..' infix                      { TEnumThen Alt $1 $2 $4 }

alternation :: { Term }
  : '<' alt  '>'                                { $2 }

bracket :: { Term }
  : '(' term ')'                                { TBracket $2 }

simple :: { Term }
  : atom                                        { $1 }
  | alternation                                 { $1 }
  | sequence                                    { $1 }
  | choice                                      { $1 }
  | stack                                       { $1 }
  | lambda                                      { $1 }
  | polyrhythm                                  { $1 }
  | repeat                                      { $1 }
  | bracket                                     { $1 }

-- special operators are left-associative
specialinfix :: { Term }
  : specialinfix specop simple           %shift { TInfix  $1 (unTok $2) $3 }
  | simple                               %shift { $1 }

-- all other operators are assumed to be right-associative, AST rotation will fix it
-- this definition is for use inside of sequences
infix :: { Term }
  : specialinfix operator infix          %shift { TInfix  $1 (unTok $2) $3 }
  | specialinfix                         %shift { $1 }

-- application is left-associative, binds stronger than operators
-- outside of sequences
app :: { Term }
  : app specialinfix                     %shift { TApp $1 $2 }
  | specialinfix                         %shift {$1}

sectionR :: { Term }
  : operator app                         %shift { TSectionR (unTok $1) $2 }

sectionL :: { Term }
  : app operator                         %shift { TSectionL $1 (unTok $2) }

-- operators outside of sequences have the weakest binding
term :: { Term }
  : app operator term                    %shift { TInfix  $1 (unTok $2) $3 }
  | app                                  %shift { $1 }
  | sectionR                             %shift { $1 }
  | sectionL                             %shift { $1 }

-------------------------------------------------------------
---------------------- parsing actions ----------------------
-------------------------------------------------------------

def :: { Def }
  : identifier many(identifier) '=' term        { Let (unTok $1) (map unTok $2) $4 }

action :: { Action }
  : string     '<-' term                        { StreamAction (unTok $1) $3 }
  | number     '<-' term                        { StreamAction (unTok $1) $3 }
  | identifier '<-' term                        { StreamSet (unTok $1) $3 }
  | ':cps' number                               { StreamSetTempo CPS (unTok $2) }
  | ':bpm' number                               { StreamSetTempo BPM (unTok $2) }
  | '!' term                                    { StreamOnce $2 }
  | ':config'                                   { ConfigPath }
  | ':resetconfig'                              { ResetConfig }
  | def                                         { Def $1 }
  | ':t' term                                   { Type $2 }
  | ':show' term                                { Show $2 }
  | ':load'                                     { Load $ unTok $1 }
  | ':info' identifier                          { Info $ unTok $2 }

actionsrecrev :: { [Action] }
  : actionsrecrev ';' action                    { $3:$1 }
  | action                                      { [$1] }

actions :: { [Action] }
  : actionsrecrev ';'                           { reverse $1 }
  | actionsrecrev                               { reverse $1 }
  |                                             { [] }

-------------------------------------------------------------
----------------------- parsing blocks ----------------------
-------------------------------------------------------------

block :: { Block }
  : some(line)                                  { toBlock $1 }

blocksrec :: { [Block] }
  : blocksrec some(bsep) block                  { $3:$1 }
  | block                                       { [$1] }

blocks :: { [Block] }
  : some(bsep) blocksrec some(bsep)             { $2 }
  | some(bsep) blocksrec                        { $2 }
  | blocksrec some(bsep)                        { $1 }
  | blocksrec                                   { $1 }

-------------------------------------------------------------
----------------------- parsing types -----------------------
-------------------------------------------------------------

atomType :: { Type }
  : textT                                       { TypeCon "Text" }
  | numT                                        { TypeCon "Number" }
  | mapT                                        { TypeCon "Map" }
  | busT                                        { TypeCon "Bus" }
  | varT                                        { TypeVar (unTok $1) }

fullType :: { Type }
  : atomType                                    { $1 }
  | fullType '->' fullType               %shift { TypeArr $1 $3 }
  | '(' fullType ')'                            { $2 }

predicate :: { Predicate }
  : classT varT                                 { IsIn (unTok $1) (TypeVar (unTok $2))}

predicates :: { [Predicate] }
  : predicate '=>'                              { [$1] }
  |                                             { [] }

scheme :: { Scheme }
  : predicates fullType                  %shift { generalize $1 $2 }


{

parseError :: (L.RangedToken, [String]) -> L.Alex a
parseError (L.RangedToken t _,poss) = do
  (L.AlexPn _ ln column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show ln <> ", column " <> show column
                <> "\n\tunexpected " <> show t
                <> "\n\texpecting " <> (intercalate "," poss)

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> Text
unTok (L.RangedToken  (L.Identifier x) _) = x
unTok (L.RangedToken  (L.Number x) _ ) = x
unTok (L.RangedToken  (L.String x) _ )= x
unTok (L.RangedToken  (L.Operator x) _) = x
unTok (L.RangedToken  (L.SpecialOp x) _) = x
unTok (L.RangedToken  (L.LoadA x) _) = x
unTok (L.RangedToken  (L.LineT x) _) = x
unTok (L.RangedToken  (L.VarToken x) _) = x
unTok (L.RangedToken  (L.TypeClass x) _) = x
unTok (L.RangedToken  (L.RepeatNum x) _) = x
unTok _ = error "can't untok"


mkAtom :: (Position -> Text -> Term) -> L.RangedToken -> L.Alex Term
mkAtom constr tok@(L.RangedToken _ range) = do
                          ed <- L.getEditorNum
                          return $ constr (toPosition ed range) (unTok tok)

toPosition :: Int -> L.Range -> Position
toPosition ed (L.Range (L.AlexPn _ line start) (L.AlexPn _ _ end)) = Pos line start end ed

toBlock :: [L.RangedToken] -> Block
toBlock [] = error "Can't happen"
toBlock xs = Block start end content
           where ls = sortOn (\(x,_) -> x) $ map (\r -> (getLn r,unTok r)) xs
                 (start, _) = head ls
                 (end, _) = last ls
                 content = Text.concat $ map snd ls
                 getLn (L.RangedToken _ (L.Range (L.AlexPn _ l _) _)) = l


parseActionsWithPos :: Int -> Int -> Text -> Either String [Action]
parseActionsWithPos ln ed input = L.runAlex input (L.setEditorNum ed >> L.setInitialLineNum ln >> pActions)

parseActions :: Text -> Either String [Action]
parseActions input = L.runAlex input pActions

parseBlocks :: Int -> Text -> Either String [Block]
parseBlocks line input = L.runAlex input (L.lineLexer >> L.setInitialLineNum line >> pBlocks)

parseScheme :: Text -> Either String Scheme
parseScheme input = L.runAlex input (L.typeLexer >> pScheme)

}
