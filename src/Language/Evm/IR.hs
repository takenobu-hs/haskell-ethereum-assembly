
module Language.Evm.IR where

------------------------------------------------------------------------
-- Internal Representation of EVM instructions
--    see yellow paper's Appendix H
------------------------------------------------------------------------

data EvmIr =
           -- 0s: Stop and Arithmetic Operations
             STOP
           | ADD
           | MUL
           | SUB
           | DIV
           | SDIV
           | MOD
           | SMOD
           | ADDMOD
           | MULMOD
           | EXP
           | SIGNEXTEND

           -- 10s: Comparison & Bitwise Logic Operations
           | LT
           | GT
           | SLT
           | SGT
           | EQ
           | ISZERO
           | AND
           | OR
           | XOR
           | NOT
           | BYTE

           -- 20s: SHA3
           | SHA3

           -- 30s: Environmental Information
           | ADDRESS
           | BALANCE
           | ORIGIN
           | CALLER
           | CALLVALUE
           | CALLDATALOAD
           | CALLDATASIZE
           | CALLDATACOPY
           | CODESIZE
           | CODECOPY
           | GASPRICE
           | EXTCODESIZE
           | EXTCODECOPY
           | RETURNDATASIZE     -- Byzantium
           | RETURNDATACOPY     -- Byzantium

           -- 40s: Block Information
           | BLOCKHASH
           | COINBASE
           | TIMESTAMP
           | NUMBER
           | DIFFICULTY
           | GASLIMIT

           -- 50s: Stack, Memory, Storage and Flow Operations
           | POP
           | MLOAD
           | MSTORE
           | MSTORE8
           | SLOAD
           | SSTORE
           | JUMP
           | JUMPI
           | PC
           | MSIZE
           | GAS
           | JUMPDEST

           -- 60s & 70s: Push Operations
           | PUSH Int Int
{---
           | PUSH1
           | PUSH2
           | PUSH3
           | PUSH4
           | PUSH5
           | PUSH6
           | PUSH7
           | PUSH8
           | PUSH9
           | PUSH10
           | PUSH11
           | PUSH12
           | PUSH13
           | PUSH14
           | PUSH15
           | PUSH16
           | PUSH17
           | PUSH18
           | PUSH19
           | PUSH20
           | PUSH21
           | PUSH22
           | PUSH23
           | PUSH24
           | PUSH25
           | PUSH26
           | PUSH27
           | PUSH28
           | PUSH29
           | PUSH30
           | PUSH31
           | PUSH32

---}

           -- 80s: Duplication Operations
           | DUP Int
{---
           | DUP1
           | DUP2
           | DUP3
           | DUP4
           | DUP5
           | DUP6
           | DUP7
           | DUP8
           | DUP9
           | DUP10
           | DUP11
           | DUP12
           | DUP13
           | DUP14
           | DUP15
           | DUP16
---}

           -- 90s: Exchange Operations
           | SWAP Int
{---
           | SWAP1
           | SWAP2
           | SWAP3
           | SWAP4
           | SWAP5
           | SWAP6
           | SWAP7
           | SWAP8
           | SWAP9
           | SWAP10
           | SWAP11
           | SWAP12
           | SWAP13
           | SWAP14
           | SWAP15
           | SWAP16
---}

           -- a0s: Logging Operations
           | LOG Int
{---
           | LOG0
           | LOG1
           | LOG2
           | LOG3
           | LOG4
---}

           -- f0s: System operations
           | CREATE
           | CALL
           | CALLCODE
           | RETURN
           | DELEGATECALL       -- Homestead

           | STATICCALL         -- Byzantium

           | INVALID
           | REVERT             -- Byzantium
           | SELFDESTRUCT

           -- pseudo instructions
           | P_JUMPDEST String  -- jump destination with symbol
           | P_JUMP String      -- jump with symbol
           | P_LABEL String     -- symbol label
           | P_PUSH String      -- push with symbol
           | P_RAW Int          -- raw byte

           deriving Show

