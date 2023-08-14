module Zwirn.Language.Block
    ( Block (..)
    , BlockError
    , getBlock
    ) where

{-
    Block.hs - parsing blocks of code and getting blocks at a specific line
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

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent :: Text
                   } deriving (Show, Eq)

type BlockError = String

getBlock :: Int -> [Block] -> Either BlockError Block
getBlock _ [] = Left "no block of code at current line"
getBlock num (block@(Block n1 n2 _):bs) = if n1 <= num && num <= n2
                                          then Right block
                                          else getBlock num bs
