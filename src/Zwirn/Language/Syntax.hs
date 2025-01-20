{-# LANGUAGE OverloadedStrings #-}

module Zwirn.Language.Syntax where

{-
    Syntax.hs - definition of the zwirn language,
    inspired by tidals mini-notation
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

import Data.Text (Text)

type Var = Text

type OperatorSymbol = Text

data Position = Pos
  { pLine :: Int,
    pStart :: Int,
    pEnd :: Int,
    pEditor :: Int
  }
  deriving (Eq, Show)

-- sugary representation of patterns
data Term
  = TVar Position Text
  | TText Position Text
  | TNum Position Text
  | TRest
  | TRepeat Term (Maybe Int)
  | TSeq [Term]
  | TStack [Term]
  | TAlt [Term]
  | TChoice Int [Term]
  | TPoly Term Term
  | TLambda [Text] Term
  | TApp Term Term
  | TInfix Term Text Term
  | TSectionR Text Term
  | TSectionL Term Text
  | TBracket Term
  deriving (Eq, Show)

data Def
  = Let Text [Text] Term
  deriving (Eq, Show)

data Tempo
  = CPS
  | BPM
  deriving (Eq, Show)

data Action
  = StreamAction Text Term
  | StreamSet Text Term
  | StreamOnce Term
  | StreamSetTempo Tempo Text
  | Config Text Text
  | ResetConfig
  | Def Def
  | Type Term
  | Show Term
  | Load Text
  | Info Text
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
