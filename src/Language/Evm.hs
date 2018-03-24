
module Language.Evm (
        EvmAsm
       ,codegen
       ,module Language.Evm.Instructions
    ) where

import           Language.Evm.Instructions
import           Language.Evm.Internal     (asm2code)
import           Language.Evm.Types


codegen = asm2code


