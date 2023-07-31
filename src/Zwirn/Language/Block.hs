module Zwirn.Language.Block
    ( Block (..)
    , getBlock
    ) where

import Data.Text (Text)

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent :: Text
                   } deriving (Show, Eq)

data BlockError = BlockError deriving (Show, Eq)

getBlock :: Int -> [Block] -> Either BlockError Block
getBlock _ [] = Left BlockError
getBlock num (block@(Block n1 n2 _):bs) = if n1 <= num && num <= n2
                                          then Right block
                                          else getBlock num bs
