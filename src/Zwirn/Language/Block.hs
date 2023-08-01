module Zwirn.Language.Block
    ( Block (..)
    , BlockError
    , getBlock
    ) where

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
