

module Test where

import           Language.Evm

main :: IO ()
main = putStrLn $ codegen prog1

prog1 :: EvmAsm
prog1 = do
    push1 0x10
    push1 0x20
    add

prog2 :: EvmAsm
prog2 = do
    prog1       -- compose
    push1 0x5
    sub
