
module Language.Evm.Opcodes where

import           Language.Evm.IR
import           Language.Evm.Types


------------------------------------------------------------------------
-- bytecode generation
------------------------------------------------------------------------
{-
-- codemap :: EvmIr -> Either String String
type EvmCode = String
-}

codemap :: EvmIr -> EvmCode
codemap ADD = "01"
codemap _   = "@@"


