
module Language.Evm.Utils where

import           Language.Evm.Types
import           Text.Printf        (printf)

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------
isByteRange :: Int -> Integer -> Bool
isByteRange nbyte x
    | x < 2^(8*nbyte) && x >= 0 = True
    | otherwise              = False

clipByte :: Int -> Integer -> EvmCode
clipByte nbyte x = reverse . take (2*nbyte) . reverse $ printf "%064x" x

calcByte :: Integer -> Int
calcByte x = floor((logBase 2 (fromInteger x)) / 8) + 1

