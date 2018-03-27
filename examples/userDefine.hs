

module Test where

import           Prelude hiding (exp)
import           Language.Evm

main :: IO ()
main = putStrLn $ codegen prog6

shiftL nbit = do    -- user defined function
    push1 nbit
    push1 2
    exp
    mul

prog6 :: EvmAsm
prog6 = do
    mload
    shiftL 16    -- using
    push1 0x1
    add



