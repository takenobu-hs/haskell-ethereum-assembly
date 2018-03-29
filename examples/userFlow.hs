

module Main where

import           Language.Evm
import           Prelude      hiding (not)

main :: IO ()
main = putStrLn $ codegen prog7

prog7 :: EvmAsm
prog7 = do
    _if (iszero)
      (do -- then
          push1 0x01
          push1 0x02
          add
      )
      (do -- else
          push1 0x01
          push1 0x02
          sub
      )


prog8 :: EvmAsm
prog8 = do
    _if (iszero) (do -- then
          push1 0x01
          push1 0x02
          add
      ) (do -- else
          push1 0x01
          push1 0x02
          sub
      )


------------------------------------------------------------------------
-- user defined control flow
------------------------------------------------------------------------

_if :: EvmAsm -> EvmAsm -> EvmAsm -> EvmAsm
_if cond thenBlock elseBlock = do

    -- define local label
    lab <- _genUniqLabel
    let _else  = "_else" ++ lab
    let _endif = "_endif" ++ lab

    -- if
    cond
    _jumpi _else

    -- then
    thenBlock
    _jump _endif

    -- else
    _label _else
    elseBlock

    -- endif
    _label _endif

