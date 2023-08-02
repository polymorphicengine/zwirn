{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Syntax where

import Data.Text (Text)

type Var = Text

type OperatorSymbol = Text

data Position = Pos { pLine :: Int
                    , pStart :: Int
                    , pEnd :: Int
                    , pEditor :: Int
                    } deriving (Eq, Show)

-- sugary representation of patterns
data Term
  = TVar Position Text
  | TText Position Text
  | TNum Position Text
  | TRest
  | TElong Term (Maybe Int)
  | TRepeat Term (Maybe Int)
  | TSeq [Term]
  | TStack [Term]
  | TAlt [Term]
  | TChoice Int [Term]
  | TEuclid Term Term Term (Maybe Term)
  | TPoly Term Term
  | TLambda [Text] Term
  | TApp Term Term
  | TInfix Term Text Term
  deriving (Eq, Show)

data Def
  = Let Text [Text] Term
  deriving (Eq, Show)

data Action
  = Stream Text Term
  | Def Def
  | Type Term
  | Show Term
  | Load Text
  | JS Term
  deriving (Eq, Show)

data Associativity
  = NonA
  | LeftA
  | RightA
  deriving (Eq, Show)

type Precedence = Int

data Fixity
  = Fixity Associativity Precedence
  deriving (Eq, Show)

type Declaration = (OperatorSymbol, Fixity)
