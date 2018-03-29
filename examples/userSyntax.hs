

module Main where

import           Prelude hiding (const)
import           Language.Evm

main :: IO ()
main = putStrLn $ codegen prog7

prog7 :: EvmAsm
prog7 = do
    let num1 = const 0x10
    let num2 = const 0x20
    let factor1 = ptr 0x05     -- for memory
    let user1   = key 0x100    -- for storage

    add2(num1, num2)
    add2(ref factor1, num1)
    add
    add2(value user1, const 3)
    memory(factor1) <== mul

    storage(user1) <== mul2(num2, (add2(const 4, ref factor1)))



------------------------------------------------------------------------
-- user define syntax and functions
------------------------------------------------------------------------

-- basic type of memory and storage
newtype Val a = Val Integer
data Ptr_
data Key_
type Ptr = Val Ptr_
type Key = Val Key_

-- store syntax
(<<=) :: EvmAsm -> EvmAsm -> EvmAsm
p1 <<= p2 = do
    p2
    p1

-- memory
ptr :: Integer -> Ptr
ptr x = Val x

ref :: Ptr -> EvmAsm
ref (Val x) = do
    _push x
    mload

memory :: Ptr -> EvmAsm
memory (Val x) = do
    _push x
    mstore

-- storage
key :: Integer -> Key
key x = Val x

value :: Key -> EvmAsm
value (Val x) = do
    _push x
    sload

storage :: Key -> EvmAsm
storage (Val x) = do
    _push x
    sstore


-- functions
const :: Integer -> EvmAsm
const x = _push x

make2op :: EvmAsm -> (EvmAsm, EvmAsm) -> EvmAsm
make2op op (x,y) = do
    y
    x
    op

add2 = make2op add
sumb = make2op sub
mul2 = make2op mul

(<==) :: EvmAsm -> EvmAsm -> EvmAsm
p1 <== p2 = do
    p2
    p1

