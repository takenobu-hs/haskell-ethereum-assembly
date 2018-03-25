
module Language.Evm.Opcodes where

import           Language.Evm.IR
import           Language.Evm.Types
import           Prelude            hiding (EQ, GT, LT)
import           Text.Printf        (printf)

------------------------------------------------------------------------
-- Bytecode mapping of EVM instructions
--    see yellow paper's Appendix H
------------------------------------------------------------------------

codemap :: EvmIr -> EvmCode

-- 0s: Stop and Arithmetic Operations
codemap STOP           = "00"
codemap ADD            = "01"
codemap MUL            = "02"
codemap SUB            = "03"
codemap DIV            = "04"
codemap SDIV           = "05"
codemap MOD            = "06"
codemap SMOD           = "07"
codemap ADDMOD         = "08"
codemap MULMOD         = "09"
codemap EXP            = "0a"
codemap SIGNEXTEND     = "0b"

-- 10s: Comparison & Bitwise Logic Operations
codemap LT             = "10"
codemap GT             = "11"
codemap SLT            = "12"
codemap SGT            = "13"
codemap EQ             = "14"
codemap ISZERO         = "15"
codemap AND            = "16"
codemap OR             = "17"
codemap XOR            = "18"
codemap NOT            = "19"
codemap BYTE           = "1a"

-- 20s: SHA3
codemap SHA3           = "20"

-- 30s: Environmental Information
codemap ADDRESS        = "30"
codemap BALANCE        = "31"
codemap ORIGIN         = "32"
codemap CALLER         = "33"
codemap CALLVALUE      = "34"
codemap CALLDATALOAD   = "35"
codemap CALLDATASIZE   = "36"
codemap CALLDATACOPY   = "37"
codemap CODESIZE       = "38"
codemap CODECOPY       = "39"
codemap GASPRICE       = "3a"
codemap EXTCODESIZE    = "3b"
codemap EXTCODECOPY    = "3c"
codemap RETURNDATASIZE = "3d"   -- Byzantium
codemap RETURNDATACOPY = "3e"   -- Byzantium

-- 40s: Block Information
codemap BLOCKHASH      = "40"
codemap COINBASE       = "41"
codemap TIMESTAMP      = "42"
codemap NUMBER         = "43"
codemap DIFFICULTY     = "44"
codemap GASLIMIT       = "45"

-- 50s: Stack, Memory, Storage and Flow Operations
codemap POP            = "50"
codemap MLOAD          = "51"
codemap MSTORE         = "52"
codemap MSTORE8        = "53"
codemap SLOAD          = "54"
codemap SSTORE         = "55"
codemap JUMP           = "56"
codemap JUMPI          = "57"
codemap PC             = "58"
codemap MSIZE          = "59"
codemap GAS            = "5a"
codemap JUMPDEST       = "5b"

-- 60s & 70s: Push Operations
-- TODO: adjust size!
codemap (PUSH n x)     = printf "%02x" (0x60 - 1 + n) ++ printf "%02x" x

-- 80s: Duplication Operations
codemap (DUP n)        = printf "%02x" (0x80 - 1 + n)

-- 90s: Exchange Operations
codemap (SWAP n)       = printf "%02x" (0x90 - 1 + n)

-- a0s: Logging Operations
codemap (LOG n)        = printf "%02x" (0xa0 - 1 + n)

-- f0s: System operations
codemap CREATE         = "f0"
codemap CALL           = "f1"
codemap CALLCODE       = "f2"
codemap RETURN         = "f3"
codemap DELEGATECALL   = "f4"     -- Byzantium

codemap STATICCALL     = "fa"     -- Byzantium

codemap INVALID        = "fd"
codemap REVERT         = "fe"     -- Homestead
codemap SELFDESTRUCT   = "ff"

