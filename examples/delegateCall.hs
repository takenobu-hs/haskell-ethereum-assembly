
--
-- Example code of delegatecall
--   by @libscott
--     https://github.com/takenobu-hs/haskell-ethereum-assembly/pull/1
--

module Main where

import           Language.Evm
import           Prelude hiding (return)

main :: IO ()
main = putStrLn $ codegen $ delegatecallCode 0x100


delegatecallCode :: Integer -> EvmAsm
delegatecallCode addr = do
  -- First thing, copy call data to memory
  calldatasize
  push1 0
  push1 0
  calldatacopy

  -- Call native contract
  push1 0       -- write buffer size
  push1 0       -- write to
  calldatasize  -- input len
  push1 0       -- read from
  push20 addr
  gas
  delegatecall  -- This pushes exit code onto the stack

  -- Copy the output to memory
  returndatasize
  push1 0
  dup1
  returndatacopy

  -- Conditionally jump to revert
  push1 0
  eq
  _jumpi "_revert"

  -- Return
  returndatasize
  push1 0
  return

  _dest "_revert"
  returndatasize
  push1 0
  revert

