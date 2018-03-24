
module Language.Evm.IR where


------------------------------------------------------------------------
-- basic type and data
------------------------------------------------------------------------
data EvmIr = STOP
           | ADD
           | PUSH Int Int
--         | PUSH1 Int
--         | PUSH2 Int
           | JUMP
           | JUMPDEST
           | P_JUMP String      -- pseudo instruction
           | P_JUMPDEST String  -- pseudo instruction
           | P_LABEL String     -- pseudo instruction
           | P_PUSH String      -- pseudo instruction
           | P_RAW Int          -- pseudo instruction
           deriving Show


