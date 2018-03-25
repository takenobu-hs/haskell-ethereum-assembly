
module Main where

import           Language.Evm

main :: IO ()
main = putStrLn $ codegen prog1

prog1 :: EvmAsm
prog1 = do
    push1 0x10
    push1 0x20
    add
    stop

