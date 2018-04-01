
module Language.Evm.Types where

import           Control.Monad.Trans.State
import           Language.Evm.IR

------------------------------------------------------------------------
-- Basic types
------------------------------------------------------------------------
type Evm a = State EvmState a
type EvmAsm = Evm ()

data EvmState = EvmState {
                  insts   :: [EvmIr]
                , labelId :: Int
                } deriving Show

type EvmCode = String

