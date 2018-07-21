
module Language.Evm.Internal where

import           Control.Monad.Trans.State
import           Language.Evm.IR
import           Language.Evm.Opcodes
import           Language.Evm.Types
import           Language.Evm.Utils


------------------------------------------------------------------------
-- Initial data
------------------------------------------------------------------------
initState :: EvmState
initState = EvmState [] 0

initProgInfo :: ProgInfo
initProgInfo = (0, [], [])


------------------------------------------------------------------------
-- Asm utility
------------------------------------------------------------------------
makeAsm :: EvmIr -> EvmAsm
makeAsm op = do
    modify $ \s -> s { insts = (insts s) ++ [op] }


------------------------------------------------------------------------
-- IR generation from Asm
------------------------------------------------------------------------
genAsmState :: EvmAsm -> EvmState
genAsmState p = execState p initState

asm2rawIr :: EvmAsm -> [EvmIr]
asm2rawIr p = insts $ genAsmState p

asm2ir :: EvmAsm -> [EvmIr]
asm2ir = resolveIr . transformIr . asm2rawIr

debugShowIr :: EvmAsm -> ProgInfo
debugShowIr p = genProgInfo initProgInfo $ asm2ir p


------------------------------------------------------------------------
-- Transform IR and resolve symbols
------------------------------------------------------------------------
type Addr = Int
type Symbol = String
type SymbolMap = [(Symbol, Addr)]
type InstMap  = [(Addr, EvmIr)]
type ProgInfo = (Addr, InstMap, SymbolMap)

-- phase1 (expantion transform)
jumpInstInc :: Int
jumpInstInc = 2      -- TODO

transformIr :: [EvmIr] -> [EvmIr]
transformIr = (>>= expantion)

expantion :: EvmIr -> [EvmIr]
expantion (P_JUMP t)  = [P_PUSH t, JUMP]
expantion (P_JUMPI t) = [P_PUSH t, JUMPI]
expantion x           = [x]


-- phase2 (symbol resolution)
--  TODO: Implement PUSH size for each length
instLen :: EvmIr -> Int
instLen (P_LABEL _) = 0
instLen (P_PUSH _)  = jumpInstInc + 1
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
        imap' = imap ++ [(pc', x)]
        smap' = addSymbolMap smap pc x
    in  genProgInfo (pc', imap', smap') xs

convSymbol :: SymbolMap -> EvmIr -> [EvmIr]
convSymbol smap (P_PUSH t) =
    case lookup t smap of
        Just x -> [PUSH jumpInstInc (toInteger x)]    -- TODO jumpInstInc
        _      -> error $ t ++ ": symbol not found"
convSymbol _ x = [x]

resolveSymbols :: SymbolMap -> [EvmIr] -> [EvmIr]
resolveSymbols smap []     = []
resolveSymbols smap (x:xs)
    | isUniqKey smap = convSymbol smap x ++ resolveSymbols smap xs
    | otherwise      = error $ (show smap) ++ ": duplicate symbol"

resolveIr :: [EvmIr] -> [EvmIr]
resolveIr x = let (_, _, smap) = genProgInfo initProgInfo x
              in  resolveSymbols smap x


------------------------------------------------------------------------
-- Bytecode generation
------------------------------------------------------------------------
ir2code :: [EvmIr] -> EvmCode
ir2code []     = ""
ir2code (x:xs) = codemap x ++ ir2code xs

asm2code :: EvmAsm -> EvmCode
asm2code = ir2code . asm2ir


------------------------------------------------------------------------
-- Special instructions and functions
------------------------------------------------------------------------
__progSize :: EvmAsm -> Integer
__progSize p = let (size, _, _) = genProgInfo initProgInfo $ asm2ir p
               in toInteger size

__genUniqLabel :: Evm String
__genUniqLabel = do
    num <- gets labelId
    modify $ \s -> s { labelId = num + 1 }
    return $ "_label_" ++ show num

