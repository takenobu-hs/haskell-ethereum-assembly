
module Language.Evm.Types where

import           Control.Monad.State
import           Language.Evm.IR

------------------------------------------------------------------------
-- Basic types
------------------------------------------------------------------------
type Evm a = State EvmState a
type EvmAsm = Evm ()

newtype EvmState = EvmState {
                  insts :: [EvmIr]
                } deriving Show

type EvmCode = String

