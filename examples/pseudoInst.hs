

module Test where

import           Language.Evm

main :: IO ()
main = putStrLn $ codegen prog2

prog2 :: EvmAsm
prog2 = do
    prog3
    prog4

prog3 :: EvmAsm
prog3 = do
    push1 0x60
    push1 0x40
    mstore
    _jump "target2"

    push1 (_progSize prog4)
    _pushlabel "top1"
    _dest "target2"


prog4 :: EvmAsm
prog4 = do
    _label "top1"
    _push 0x11234456788
    _raw 0x7
    _raw 0x8




