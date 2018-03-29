

module Test where

import           Language.Evm

main :: IO ()
main = putStrLn $ codegen prog2

prog2 :: EvmAsm
prog2 = do
    prog2a
    prog2b

prog2a = do
    push1 0x40
    mload

prog2b = do
    push1 0x20
    add

