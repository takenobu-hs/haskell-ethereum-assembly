

module Test where

import           Language.Evm

main :: IO ()
main = putStrLn $ codegen prog5

isBizantinum = True :: Bool

prog5 :: EvmAsm
prog5 = do
    if isBizantinum          -- if expression on Haskell
        then prog_header1
        else prog_header2
    push1 0x60
    push1 0x40
    mstore

prog_header1 :: EvmAsm
prog_header1 = do
    add

prog_header2 :: EvmAsm
prog_header2 = do
    sub
