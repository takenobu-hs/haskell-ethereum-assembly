
module Language.Evm.Instructions where

import           Language.Evm.Internal
import           Language.Evm.IR
import           Language.Evm.Types


------------------------------------------------------------------------
-- special instructions and functions
------------------------------------------------------------------------
-- pseudo instructions
_label :: String -> EvmAsm
_label x = makeAsm $ P_LABEL x

_dest :: String -> EvmAsm
_dest x = makeAsm $ P_JUMPDEST x

_raw :: Int -> EvmAsm
_raw x = makeAsm $ P_RAW x


-- macro instruction
_jump :: String -> EvmAsm
_jump x = makeAsm $ P_JUMP x

_push :: String -> EvmAsm
_push x = makeAsm $ P_PUSH x


-- built-in function
_codeSize :: EvmAsm -> Int
_codeSize = __codeSize


------------------------------------------------------------------------
-- definition of asm
------------------------------------------------------------------------
stop :: EvmAsm
stop = makeAsm STOP

add :: EvmAsm
add = makeAsm ADD

push1 :: Int -> EvmAsm
push1 x = makeAsm $ PUSH 1 x

push2 :: Int -> EvmAsm
push2 x = makeAsm $ PUSH 2 x

jump :: EvmAsm
jump = makeAsm JUMP


