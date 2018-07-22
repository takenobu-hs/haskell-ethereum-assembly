
module Language.Evm.Ppr where

import           Data.Char             (toLower)
import           Language.Evm.Internal
import           Language.Evm.IR
import           Language.Evm.Opcodes
import           Language.Evm.Types
import           Text.Printf           (printf)


------------------------------------------------------------------------
-- pretty print for simple list
------------------------------------------------------------------------

-- TODO: Should refactoring!
--       This is a temporary implementation model for debug.

pprList :: EvmAsm -> String
pprList = unlines . pprList'

pprList' :: EvmAsm -> [String]
pprList' p = let (_, imap, smap) = genProgInfo initProgInfo $ asm2ir p
             in map pprList'' imap

pprList'' :: (Addr, EvmIr) -> String
pprList'' (addr, x) = printf "%06x: %s" addr (pprIrList x)

pprIrList :: EvmIr -> String
pprIrList (P_JUMPDEST x) = printf "jumpdest"
-- pprIrList (P_JUMP x)  = printf "jump(%s)" x
-- pprIrList (P_JUMPI x) = printf "%s\njumpi" x
pprIrList (P_LABEL x)    = printf "%s:" x
pprIrList (P_RAW x)      = printf "// 0x%x" x
pprIrList (PUSH n x)     = printf "push%d 0x%x" n x
pprIrList (DUP x)        = printf "dpu%d" x
pprIrList (SWAP x)       = printf "swap%d" x
pprIrList (LOG x)        = printf "log%d" x
pprIrList x              = map toLower $ show x

