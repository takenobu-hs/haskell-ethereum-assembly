
module Language.Evm.Instructions where

import           Language.Evm.Internal
import           Language.Evm.IR
import           Language.Evm.Types
import           Prelude               hiding (EQ, GT, LT)


------------------------------------------------------------------------
-- Special instructions and functions
------------------------------------------------------------------------

-- pseudo instructions
_label :: String -> EvmAsm
_label x = makeAsm $ P_LABEL x

_dest :: String -> EvmAsm
_dest x = makeAsm $ P_JUMPDEST x

_raw :: Int -> EvmAsm
_raw x = makeAsm $ P_RAW x


-- macro instructions
_jump :: String -> EvmAsm
_jump x = makeAsm $ P_JUMP x

_push :: String -> EvmAsm
_push x = makeAsm $ P_PUSH x


-- built-in function
_codeSize :: EvmAsm -> Integer
_codeSize = __codeSize


------------------------------------------------------------------------
-- Definition of asm
------------------------------------------------------------------------

-- 0s: Stop and Arithmetic Operations
stop, add, mul, sub, div, sdiv :: EvmAsm
mod, smod, addmod, mulmod, exp, signextend :: EvmAsm
stop       = makeAsm STOP
add        = makeAsm ADD
mul        = makeAsm MUL
sub        = makeAsm SUB
div        = makeAsm DIV
sdiv       = makeAsm SDIV
mod        = makeAsm MOD
smod       = makeAsm SMOD
addmod     = makeAsm ADDMOD
mulmod     = makeAsm MULMOD
exp        = makeAsm EXP
signextend = makeAsm SIGNEXTEND

-- 10s: Comparison & Bitwise Logic Operations
lt, gt, slt, sgt, eq, iszero, and, or, xor, not, byte :: EvmAsm
lt     = makeAsm LT
gt     = makeAsm GT
slt    = makeAsm SLT
sgt    = makeAsm SGT
eq     = makeAsm EQ
iszero = makeAsm ISZERO
and    = makeAsm AND
or     = makeAsm OR
xor    = makeAsm XOR
not    = makeAsm NOT
byte   = makeAsm BYTE

-- 20s: SHA3
sha3 :: EvmAsm
sha3 = makeAsm SHA3

-- 30s: Environmental Information
address, balance, origin, caller, callvalue, calldataload :: EvmAsm
calldatasize, calldatacopy, codesize, codecopy :: EvmAsm
gasprice, extcodesize, extcodecopy, returndatasize, returndatacopy :: EvmAsm
address        = makeAsm ADDRESS
balance        = makeAsm BALANCE
origin         = makeAsm ORIGIN
caller         = makeAsm CALLER
callvalue      = makeAsm CALLVALUE
calldataload   = makeAsm CALLDATALOAD
calldatasize   = makeAsm CALLDATASIZE
calldatacopy   = makeAsm CALLDATACOPY
codesize       = makeAsm CODESIZE
codecopy       = makeAsm CODECOPY
gasprice       = makeAsm GASPRICE
extcodesize    = makeAsm EXTCODESIZE
extcodecopy    = makeAsm EXTCODECOPY
returndatasize = makeAsm RETURNDATASIZE
returndatacopy = makeAsm RETURNDATACOPY

-- 40s: Block Information
blockhash, coinbase, timestamp, number, difficulty, gaslimit :: EvmAsm
blockhash  = makeAsm BLOCKHASH
coinbase   = makeAsm COINBASE
timestamp  = makeAsm TIMESTAMP
number     = makeAsm NUMBER
difficulty = makeAsm DIFFICULTY
gaslimit   = makeAsm GASLIMIT

-- 50s: Stack, Memory, Storage and Flow Operations
pop, mload, mstore, mstore8, sload, sstore :: EvmAsm
jump, jumpi, pc, msize, gas, jumpdest :: EvmAsm
pop      = makeAsm POP
mload    = makeAsm MLOAD
mstore   = makeAsm MSTORE
mstore8  = makeAsm MSTORE8
sload    = makeAsm SLOAD
sstore   = makeAsm SSTORE
jump     = makeAsm JUMP
jumpi    = makeAsm JUMPI
pc       = makeAsm PC
msize    = makeAsm MSIZE
gas      = makeAsm GAS
jumpdest = makeAsm JUMPDEST

-- 60s & 70s: Push Operations
push1, push2, push3, push4, push5, push6 :: Integer -> EvmAsm
push7, push8, push9, push10, push11, push12 :: Integer -> EvmAsm
push13, push14, push15, push16, push17, push18 :: Integer -> EvmAsm
push19, push20, push21, push22, push23, push24 :: Integer -> EvmAsm
push25, push26, push27, push28, push29, push30 :: Integer -> EvmAsm
push31, push32 :: Integer -> EvmAsm
push1 x  = makeAsm $ PUSH  1 x
push2 x  = makeAsm $ PUSH  2 x
push3 x  = makeAsm $ PUSH  3 x
push4 x  = makeAsm $ PUSH  4 x
push5 x  = makeAsm $ PUSH  5 x
push6 x  = makeAsm $ PUSH  6 x
push7 x  = makeAsm $ PUSH  7 x
push8 x  = makeAsm $ PUSH  8 x
push9 x  = makeAsm $ PUSH  9 x
push10 x = makeAsm $ PUSH 10 x
push11 x = makeAsm $ PUSH 11 x
push12 x = makeAsm $ PUSH 12 x
push13 x = makeAsm $ PUSH 13 x
push14 x = makeAsm $ PUSH 14 x
push15 x = makeAsm $ PUSH 15 x
push16 x = makeAsm $ PUSH 16 x
push17 x = makeAsm $ PUSH 17 x
push18 x = makeAsm $ PUSH 18 x
push19 x = makeAsm $ PUSH 19 x
push20 x = makeAsm $ PUSH 20 x
push21 x = makeAsm $ PUSH 21 x
push22 x = makeAsm $ PUSH 22 x
push23 x = makeAsm $ PUSH 23 x
push24 x = makeAsm $ PUSH 24 x
push25 x = makeAsm $ PUSH 25 x
push26 x = makeAsm $ PUSH 26 x
push27 x = makeAsm $ PUSH 27 x
push28 x = makeAsm $ PUSH 28 x
push29 x = makeAsm $ PUSH 29 x
push30 x = makeAsm $ PUSH 30 x
push31 x = makeAsm $ PUSH 31 x
push32 x = makeAsm $ PUSH 32 x

-- 80s: Duplication Operations
dup1, dup2, dup3, dup4, dup5, dup6, dup7, dup8 :: EvmAsm
dup9, dup10, dup11, dup12, dup13, dup14, dup15, dup16 :: EvmAsm
dup1  = makeAsm $ DUP  1
dup2  = makeAsm $ DUP  2
dup3  = makeAsm $ DUP  3
dup4  = makeAsm $ DUP  4
dup5  = makeAsm $ DUP  5
dup6  = makeAsm $ DUP  6
dup7  = makeAsm $ DUP  7
dup8  = makeAsm $ DUP  8
dup9  = makeAsm $ DUP  9
dup10 = makeAsm $ DUP 10
dup11 = makeAsm $ DUP 11
dup12 = makeAsm $ DUP 12
dup13 = makeAsm $ DUP 13
dup14 = makeAsm $ DUP 14
dup15 = makeAsm $ DUP 15
dup16 = makeAsm $ DUP 16

-- 90s: Exchange Operations
swap1, swap2, swap3, swap4, swap5, swap6, swap7, swap8 :: EvmAsm
swap9, swap10, swap11, swap12, swap13, swap14, swap15, swap16 :: EvmAsm
swap1  = makeAsm $ SWAP  1
swap2  = makeAsm $ SWAP  2
swap3  = makeAsm $ SWAP  3
swap4  = makeAsm $ SWAP  4
swap5  = makeAsm $ SWAP  5
swap6  = makeAsm $ SWAP  6
swap7  = makeAsm $ SWAP  7
swap8  = makeAsm $ SWAP  8
swap9  = makeAsm $ SWAP  9
swap10 = makeAsm $ SWAP 10
swap11 = makeAsm $ SWAP 11
swap12 = makeAsm $ SWAP 12
swap13 = makeAsm $ SWAP 13
swap14 = makeAsm $ SWAP 14
swap15 = makeAsm $ SWAP 15
swap16 = makeAsm $ SWAP 16

-- a0s: Logging Operations
log0, log1, log2, log3, log4 :: EvmAsm
log0 = makeAsm $ LOG 0
log1 = makeAsm $ LOG 1
log2 = makeAsm $ LOG 2
log3 = makeAsm $ LOG 3
log4 = makeAsm $ LOG 4

-- f0s: System operations
create, call, callcode, return, delegatecall :: EvmAsm
create       = makeAsm CREATE
call         = makeAsm CALL
callcode     = makeAsm CALLCODE
return       = makeAsm RETURN
delegatecall = makeAsm DELEGATECALL

staticcall :: EvmAsm
staticcall  = makeAsm STATICCALL

invalid, revert, selfdestruct :: EvmAsm
invalid      = makeAsm INVALID
revert       = makeAsm REVERT
selfdestruct = makeAsm SELFDESTRUCT

