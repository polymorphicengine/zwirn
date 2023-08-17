{
{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Parser
    ( parseActionsWithPos
    , parseActions
    , parseBlocks
    , parseTypeDecls
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
%name pTypeDecls typeDecls
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
  -- Elongation
  '@'             { L.RangedToken L.Elongate _ }
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
  -- Polyrhythm
  '%'             { L.RangedToken L.Poly _ }
  -- Euclid
  '{'             { L.RangedToken L.LBraces _ }
  '}'             { L.RangedToken L.RBraces _ }
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
  ':js'           { L.RangedToken L.JSA _ }
  -- Type Tokens
  '::'       { L.RangedToken L.DoubleColon _ }
  typefam    { L.RangedToken L.PTypeFam _ }
  '=>'       { L.RangedToken L.Context _ }
  textT      { L.RangedToken L.TextToken _ }
  numT       { L.RangedToken L.NumberToken _ }
  controlT   { L.RangedToken L.ControlToken _ }
  varT       { L.RangedToken (L.VarToken _) _ }
  classT     { L.RangedToken (L.TypeClass _ _) _ }

%%

optional(p)
  :   { Nothing }
  | p { Just $1 }

many_rev(p)
  :               { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

some_rev(p)
  : p             { [$1] }
  | some_rev(p) p { $2 : $1 }

some(p)
  : some_rev(p) { reverse $1 }

sepBy_rev(p, sep)
  : p                      { [$1] }
  | sepBy_rev(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep) { reverse $1 }

atom :: { Term }
  : identifier { % (mkAtom TVar) $1 }
  | number     { % (mkAtom TNum) $1 }
  | string     { % (mkAtom TText) $1 }
  | '~'        { TRest }

sequence :: { Term }
  : some(simpleSeq) %shift { TSeq $1 }

sequence2 :: { Term }
  : some(simpleSeq) %shift { TSeq $1 }

stack :: { [Term] }
  : sepBy(sequence, ',')   { $1 }

choice :: { [Term] }
  : sepBy(sequence2, '|')  { $1 }

lambda :: { Term }
  : '\\' some(identifier) '->' term %shift { TLambda (map unTok $2) $4 }

polyrhythm :: { Term }
  : simple '%' term     %shift  { TPoly $1 $3 }

elongate :: { Term }
  : simple '@' number           { TElong $1 (Just $ read $ Text.unpack $ unTok $3) }
  | simple '@'          %shift  { TElong $1 Nothing }

repeat :: { Term }
  : simple '!' number           { TRepeat $1 (Just $ read $ Text.unpack $ unTok $3) }
  | simple '!'          %shift  { TRepeat $1 Nothing }

fullSequence :: { Term }
  : '[' stack ']'            { TStack $2 }
  | '[' choice ']'           { % L.increaseChoice >>= \x -> return $ TChoice x $2 }
  | '[' some(simpleSeq) ']'     { TSeq $2 }

alternation :: { Term }
  : '<' some(simpleSeq) '>' { TAlt $2 }

euclid :: { Term }
  : simple '{' term ',' term '}'           { TEuclid  $1 $3 $5 Nothing }
  | simple '{' term ',' term ',' term '}'  { TEuclid  $1 $3 $5 (Just $7) }

simple :: { Term }
  : atom                           {$1}
  | alternation                    {$1}
  | fullSequence                   {$1}
  | lambda                         {$1}
  | polyrhythm                     {$1}
  | elongate                       {$1}
  | repeat                         {$1}
  | euclid                         {$1}
  | specialinfix                   {$1}
  | '(' term ')'                   { TBracket $2 }

specialinfix :: { Term }
  : simple specop simple    %shift { TInfix  $1 (unTok $2) $3 }

simpleinfix :: { Term }
  : simple operator simple  %shift { TInfix  $1 (unTok $2) $3 }

simpleSeq :: { Term }
  : simpleinfix             %shift {$1}
  | simple                  %shift {$1}

app :: { Term }
  : app simple              %shift { TApp $1 $2 }
  | simple                  %shift {$1}

term :: { Term }
  : app operator term       %shift { TInfix  $1 (unTok $2) $3 }
  | app                     %shift {$1}

-- parsing definitions

def :: { Def }
  : identifier many(identifier) '=' term { Let (unTok $1) (map unTok $2) $4 }

defs :: { [Def] }
  : sepBy(def, ';')        {$1}

action :: { Action }
  : string     '<-' term              { Stream (unTok $1) $3 }
  | number     '<-' term              { Stream (unTok $1) $3 }
  | identifier '<-' term              { StreamSet (unTok $1) $3 }
  | ':cps' term                       { StreamSetTempo CPS $2 }
  | ':bpm' term                       { StreamSetTempo BPM $2 }
  | '!' term                          { StreamOnce $2 }
  | ':config' identifier string       { Config (unTok $2) (unTok $3) }
  | ':config' identifier identifier   { Config (unTok $2) (unTok $3) }
  | ':config' identifier number       { Config (unTok $2) (unTok $3) }
  | ':resetconfig'                    { ResetConfig }
  | def                               { Def $1 }
  | ':t' term                         { Type $2 }
  | ':show' term                      { Show $2 }
  | ':load'                           { Load $ unTok $1 }
  | ':js' term                        { JS $2 }

actions :: { [Action] }
  : sepBy(action, ';')                {$1}

-- parsing blocks of text

block :: { Block }
  : some(line)                                      {toBlock $1}

blocksrec :: { [Block] }
  : blocksrec some(bsep) block                      {$3:$1}
  | block                                           {[$1]}

blocks :: { [Block] }
  : some(bsep) blocksrec some(bsep)                 {$2}
  | some(bsep) blocksrec                            {$2}
  | blocksrec some(bsep)                            {$1}
  | blocksrec                                       {$1}

atomType :: { Type }
  : textT                                           { TypeCon "Text" }
  | numT                                            { TypeCon "Number" }
  | controlT                                        { TypeCon "ValueMap" }
  | varT                                            { TypeVar (unTok $1) }

fullType :: { Type }
  : atomType                                        { $1 }
  | fullType '->' fullType                %shift    { TypeArr $1 $3 }
  | '(' fullType ')'                                { $2 }

predicate :: { Predicate }
  : classT                                          { mkPred $1 }

predicates :: { [Predicate] }
  : '(' sepBy(predicate, ',') ')' '=>'              {$2}
  | predicate '=>'                                  {[$1]}
  |                                                 {[]}

scheme :: { Scheme }
  : predicates typefam fullType                     {generalize $1 $3}

typeDecl :: { (Text,Scheme) }
  : identifier '::' scheme                          {(unTok $1, $3)}
  | '(' operator ')' '::' scheme                    {(unTok $2, $5)}
  | '(' specop ')' '::' scheme                      {(unTok $2, $5)}

typeDecls :: { [(Text,Scheme)] }
  : some(typeDecl)                                  {map (\(x,y) -> (x, filterPatClass y)) $1}

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
unTok _ = error "can't untok"

mkPred :: L.RangedToken -> Predicate
mkPred (L.RangedToken (L.TypeClass c x) _) = IsIn c (TypeVar x)
mkPred _ = error "can't make predicate"

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

parseTypeDecls :: Text -> Either String [(Text,Scheme)]
parseTypeDecls input = L.runAlex input (L.typeLexer >> pTypeDecls)

}
