
module Language.Evm.Types where

import           Control.Monad.State
import           Language.Evm.IR


------------------------------------------------------------------------
-- basic type and data
------------------------------------------------------------------------
type Evm a = State EvmState a
type EvmAsm = Evm ()

-- data EvmState = EvmState {
newtype EvmState = EvmState {
                  insts :: [EvmIr]
                } deriving Show

-- codemap :: EvmIr -> Either String String
type EvmCode = String

