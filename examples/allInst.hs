
module Main where

import           Language.Evm
import           Prelude hiding (div, mod, exp, and, or, not, return)

main :: IO ()
main = putStrLn $ codegen prog

prog :: EvmAsm
prog = do
    stop
    add
    mul
    sub
    div
    sdiv
    mod
    smod
    addmod
    mulmod
    exp
    signextend
{---
    missing opcode 0xc
    missing opcode 0xd
    missing opcode 0xe
    missing opcode 0xf
---}
    lt
    gt
    slt
    sgt
    eq
    iszero
    and
    or
    xor
    not
    byte
{---
    missing opcode 0x1b
    missing opcode 0x1c
    missing opcode 0x1d
    missing opcode 0x1e
    missing opcode 0x1f
---}
    sha3
{---
    missing opcode 0x21
    missing opcode 0x22
    missing opcode 0x23
    missing opcode 0x24
    missing opcode 0x25
    missing opcode 0x26
    missing opcode 0x27
    missing opcode 0x28
    missing opcode 0x29
    missing opcode 0x2a
    missing opcode 0x2b
    missing opcode 0x2c
    missing opcode 0x2d
    missing opcode 0x2e
    missing opcode 0x2f
---}
    address
    balance
    origin
    caller
    callvalue
    calldataload
    calldatasize
    calldatacopy
    codesize
    codecopy
    gasprice
    extcodesize
    extcodecopy
    returndatasize
    returndatacopy
{---
    missing opcode 0x3f
---}
    blockhash
    coinbase
    timestamp
    number
    difficulty
    gaslimit
{---
    missing opcode 0x46
    missing opcode 0x47
    missing opcode 0x48
    missing opcode 0x49
    missing opcode 0x4a
    missing opcode 0x4b
    missing opcode 0x4c
    missing opcode 0x4d
    missing opcode 0x4e
    missing opcode 0x4f
---}
    pop
    mload
    mstore
    mstore8
    sload
    sstore
    jump
    jumpi
    pc
    msize
    gas
    jumpdest
{---
    missing opcode 0x5c
    missing opcode 0x5d
    missing opcode 0x5e
    missing opcode 0x5f
---}
    push1 0x61
    push3 0x636465
    push7 0x6768696a6b6c6d
    push15 0x6f707172737475767778797a7b7c7d
    push31 0x7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d
    swap15
    swap16
    log0
    log1
    log2
    log3
    log4
{-
    missing opcode 0xa5
    missing opcode 0xa6
    missing opcode 0xa7
    missing opcode 0xa8
    missing opcode 0xa9
    missing opcode 0xaa
    missing opcode 0xab
    missing opcode 0xac
    missing opcode 0xad
    missing opcode 0xae
    missing opcode 0xaf
---}
{---
    push
    dup
    swap
---}
{---
    missing opcode 0xb3
    missing opcode 0xb4
    missing opcode 0xb5
    missing opcode 0xb6
    missing opcode 0xb7
    missing opcode 0xb8
    missing opcode 0xb9
    missing opcode 0xba
    missing opcode 0xbb
    missing opcode 0xbc
    missing opcode 0xbd
    missing opcode 0xbe
    missing opcode 0xbf
    missing opcode 0xc0
    missing opcode 0xc1
    missing opcode 0xc2
    missing opcode 0xc3
    missing opcode 0xc4
    missing opcode 0xc5
    missing opcode 0xc6
    missing opcode 0xc7
    missing opcode 0xc8
    missing opcode 0xc9
    missing opcode 0xca
    missing opcode 0xcb
    missing opcode 0xcc
    missing opcode 0xcd
    missing opcode 0xce
    missing opcode 0xcf
    missing opcode 0xd0
    missing opcode 0xd1
    missing opcode 0xd2
    missing opcode 0xd3
    missing opcode 0xd4
    missing opcode 0xd5
    missing opcode 0xd6
    missing opcode 0xd7
    missing opcode 0xd8
    missing opcode 0xd9
    missing opcode 0xda
    missing opcode 0xdb
    missing opcode 0xdc
    missing opcode 0xdd
    missing opcode 0xde
    missing opcode 0xdf
    missing opcode 0xe0
    missing opcode 0xe1
    missing opcode 0xe2
    missing opcode 0xe3
    missing opcode 0xe4
    missing opcode 0xe5
    missing opcode 0xe6
    missing opcode 0xe7
    missing opcode 0xe8
    missing opcode 0xe9
    missing opcode 0xea
    missing opcode 0xeb
    missing opcode 0xec
    missing opcode 0xed
    missing opcode 0xee
    missing opcode 0xef
---}
    create
    call
    callcode
    return
    delegatecall
{---
    missing opcode 0xf5
    missing opcode 0xf6
    missing opcode 0xf7
    missing opcode 0xf8
    missing opcode 0xf9
---}
    staticcall
{---
    missing opcode 0xfb
    missing opcode 0xfc
---}
    revert
{---
    missing opcode 0xfe
---}
    invalid
    selfdestruct

