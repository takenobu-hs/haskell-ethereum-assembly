
module Language.Evm.Internal where

import           Control.Monad.State
import           Language.Evm.IR
import           Language.Evm.Opcodes
import           Language.Evm.Types
import           Language.Evm.Utils


------------------------------------------------------------------------
-- Initial data
------------------------------------------------------------------------
initState :: EvmState
initState = EvmState []

initProgInfo :: ProgInfo 
initProgInfo = (0, [], [])


------------------------------------------------------------------------
-- Asm utility
------------------------------------------------------------------------
makeAsm :: EvmIr -> EvmAsm
makeAsm op = do
    i <- gets insts
    modify $ \s -> s { insts = i ++ [op] }


------------------------------------------------------------------------
-- Pre-processing
------------------------------------------------------------------------
genAsmState :: EvmAsm -> EvmState
genAsmState p = execState p initState

asm2ir :: EvmAsm -> [EvmIr]
asm2ir p = insts $ genAsmState p


------------------------------------------------------------------------
-- Pre-processing
------------------------------------------------------------------------
type Addr = Int
type Symbol = String
type SymbolMap = [(Symbol, Addr)]
type InstMap  = [(Addr, EvmIr)]
type ProgInfo = (Addr, InstMap, SymbolMap)

-- pre-phase1 (code analysis)
instLen :: EvmIr -> Int
instLen (P_LABEL _) = 0
instLen (PUSH n _)  = n + 1
instLen _           = 1

addSymbolMap :: SymbolMap -> Addr -> EvmIr -> SymbolMap
addSymbolMap smap pc (P_JUMPDEST s) = (s, pc) : smap
addSymbolMap smap pc (P_LABEL s)    = (s, pc) : smap
addSymbolMap smap _ _               = smap

genProgInfo :: ProgInfo -> [EvmIr] -> ProgInfo
genProgInfo v [] = v
genProgInfo (pc, imap, smap) (x:xs) =
    let pc'   = instLen x + pc
        imap' = imap ++ [(pc, x)]
        smap' = addSymbolMap smap pc x
    in  genProgInfo (pc', imap', smap') xs

debugShowIr :: EvmAsm -> ProgInfo
debugShowIr x = genProgInfo initProgInfo $ asm2ir x


-- pre-phase2 (symbol resolution)
--  TODO: Implement PUSH size for each length
convSymbol :: SymbolMap -> EvmIr -> [EvmIr]
convSymbol smap (P_JUMP t) =
    case lookup t smap of
        Just x -> [PUSH 2 (toInteger x), JUMP]
        _      -> error $ t ++ ": symbol not found"

convSymbol smap (P_JUMPI t) =
    case lookup t smap of
        Just x -> [PUSH 2 (toInteger x), JUMPI]
        _      -> error $ t ++ ": symbol not found"

convSymbol smap (P_PUSH t) =
    case lookup t smap of
        Just x -> [PUSH 2 (toInteger x)]
        _      -> error $ t ++ ": symbol not found"

convSymbol _ x = [x]

resolveSymbols :: SymbolMap -> [EvmIr] -> [EvmIr]
resolveSymbols smap []     = []
resolveSymbols smap (x:xs)
    | isUniqKey smap = convSymbol smap x ++ resolveSymbols smap xs
    | otherwise      = error $ (show smap) ++ ": duplicate symbol"

ir2ir :: [EvmIr] -> [EvmIr]
ir2ir x = let (_, _, smap) = genProgInfo initProgInfo x
          in  resolveSymbols smap x


------------------------------------------------------------------------
-- Bytecode generation
------------------------------------------------------------------------
ir2code :: [EvmIr] -> EvmCode
ir2code []     = ""
ir2code (x:xs) = codemap x ++ ir2code xs

asm2code :: EvmAsm -> EvmCode
asm2code = ir2code . ir2ir . asm2ir


------------------------------------------------------------------------
-- Special instructions and functions
------------------------------------------------------------------------
__progSize :: EvmAsm -> Integer
__progSize p = let (size, _, _) = genProgInfo initProgInfo
                               . insts . execState p $ initState
               in toInteger size

