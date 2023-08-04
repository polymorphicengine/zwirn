{
module Zwirn.Language.Parser
    ( parseActionsWithPos
    , parseActions
    , parseBlocks
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.List (intercalate, sortOn)

import qualified Zwirn.Language.Lexer as L
import Zwirn.Language.Syntax
import Zwirn.Language.Block
}

%name parse term
%name pActions actions
%name pBlocks blocks
%tokentype { L.RangedToken }
%errorhandlertype explist
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
  -- Identifiers
  identifier { L.RangedToken (L.Identifier _) _ }
  -- Operators
  operator   { L.RangedToken (L.Operator _) _ }
  -- Constants
  string     { L.RangedToken (L.String _) _ }
  number     { L.RangedToken (L.Number _) _ }
  line       { L.RangedToken (L.LineT _) _ }
  bsep       { L.RangedToken (L.BlockSep) _ }
  '~'        { L.RangedToken L.Rest _ }
  -- Repeat
  '!'        { L.RangedToken L.Repeat _ }
  -- Elongation
  '@'        { L.RangedToken L.Elongate _ }
  -- Parenthesis
  '('        { L.RangedToken L.LPar _ }
  ')'        { L.RangedToken L.RPar _ }
  -- Sequences
  '['        { L.RangedToken L.LBrack _ }
  ']'        { L.RangedToken L.RBrack _ }
  -- Stacks
  ','        { L.RangedToken L.Comma _ }
  -- Alternations
  '<'        { L.RangedToken L.LAngle _ }
  '>'        { L.RangedToken L.RAngle _ }
  -- Choice
  '|'        { L.RangedToken L.Pipe _ }
  -- Polyrhythm
  '%'        { L.RangedToken L.Poly _ }
  -- Euclid
  '{'        { L.RangedToken L.LBraces _ }
  '}'        { L.RangedToken L.RBraces _ }
  -- Lambda
  '\\'       { L.RangedToken L.Lambda _ }
  '->'       { L.RangedToken L.Arrow _ }
  -- Actions
  ';'        { L.RangedToken L.Colon _ }
  '<-'       { L.RangedToken L.StreamA _ }
  ':t'       { L.RangedToken L.TypeA _ }
  ':show'       { L.RangedToken L.ShowA _ }
  '='        { L.RangedToken L.Assign _ }
  ':load'    { L.RangedToken (L.LoadA _ ) _}
  ':js'      { L.RangedToken L.JSA _ }

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
  | '(' term ')'                   {$2}

simpleinfix :: { Term }
  : simple operator simple  %shift { TInfix  $1 (unTok $2) $3 }

infix :: { Term }
  : simple operator term    %shift { TInfix  $1 (unTok $2) $3 }

simpleApp :: { Term }
  : infix                   %shift {$1}
  | simple                  %shift {$1}

simpleSeq :: { Term }
  : simpleinfix             %shift {$1}
  | simple                  %shift {$1}

term :: { Term }
  : term simpleApp          %shift { TApp $1 $2 }
  | simpleApp               %shift {$1}

-- parsing definitions

def :: { Def }
  : identifier many(identifier) '=' term { Let (unTok $1) (map unTok $2) $4 }

defs :: { [Def] }
  : sepBy(def, ';')        {$1}

action :: { Action }
  : identifier '<-' term   { Stream (unTok $1) $3 }
  | number     '<-' term   { Stream (unTok $1) $3 }
  | def                    { Def $1 }
  | ':t' term              { Type $2 }
  | ':show' term           { Show $2 }
  | ':load'                { Load $ unTok $1 }
  | ':js' term             { JS $2 }

actions :: { [Action] }
  : sepBy(action, ';')           {$1}

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

{
parseError :: (L.RangedToken, [String]) -> L.Alex a
parseError (L.RangedToken t _,poss) = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column
                <> "\n\tunexpected " <> show t
                <> "\n\texpecting " <> (intercalate "," poss)

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> Text
unTok (L.RangedToken  (L.Identifier x) _) = x
unTok (L.RangedToken  (L.Number x) _ ) = x
unTok (L.RangedToken  (L.String x) _ )= x
unTok (L.RangedToken  (L.Operator x) _) = x
unTok (L.RangedToken  (L.LoadA x) _) = x
unTok (L.RangedToken  (L.LineT x) _) = x
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

}
