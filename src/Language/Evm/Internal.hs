
module Language.Evm.Internal where

import           Control.Monad.State
import           Language.Evm.IR
import           Language.Evm.Opecode
import           Language.Evm.Types


------------------------------------------------------------------------
-- basic type and data
------------------------------------------------------------------------
{-
type Evm a = State EvmState a
type EvmAsm = Evm ()
-}

{-
-- data EvmState = EvmState {
newtype EvmState = EvmState {
                  insts :: [EvmIr]
                } deriving Show
-}

initState :: EvmState
initState = EvmState []

{-
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
-}


------------------------------------------------------------------------
-- asm utility
------------------------------------------------------------------------
makeAsm :: EvmIr -> EvmAsm
makeAsm op = do
    i <- gets insts
    modify $ \s -> s { insts = i ++ [op] }



------------------------------------------------------------------------
-- pre-processing
------------------------------------------------------------------------
genAsmState :: EvmAsm -> EvmState
genAsmState p = execState p initState

asm2ir :: EvmAsm -> [EvmIr]
asm2ir p = insts $ genAsmState p


------------------------------------------------------------------------
-- pre-processing
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
instLen _           = 1 -- tmp

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
debugShowIr x = genProgInfo (0, [], []) $ asm2ir x


-- pre-phase2 (symbol resolution)
convSymbol :: SymbolMap -> EvmIr -> [EvmIr]
convSymbol smap (P_JUMP t) =
    case lookup t smap of
        Just x -> [PUSH 2 x, JUMP]
        _      -> error $ t ++ ": symbol not found"

convSymbol smap (P_PUSH t) =
    case lookup t smap of
        Just x -> [PUSH 2 x]
        _      -> error $ t ++ ": symbol not found"

convSymbol _ x = [x]

resolveSymbols :: SymbolMap -> [EvmIr] -> [EvmIr]
resolveSymbols smap []     = []
resolveSymbols smap (x:xs) = convSymbol smap x ++ resolveSymbols smap xs
-- resolveSymbols smap = foldr ((++) . convSymbol smap) []

ir2ir :: [EvmIr] -> [EvmIr]
ir2ir x = let (_, _, smap) = genProgInfo (0, [], []) x
          in  resolveSymbols smap x


------------------------------------------------------------------------
-- bytecode generation
------------------------------------------------------------------------
{-
-- codemap :: EvmIr -> Either String String
type EvmCode = String

codemap :: EvmIr -> EvmCode
codemap ADD = "01"
codemap _   = "@@"
-}

ir2code :: [EvmIr] -> EvmCode
ir2code []     = ""
ir2code (x:xs) = codemap x ++ ir2code xs
-- ir2code = foldr ((++) . codemap) ""

asm2code :: EvmAsm -> EvmCode
asm2code = ir2code . ir2ir . asm2ir

{-
codegen :: EvmAsm -> EvmCode
codegen = asm2code
-}


------------------------------------------------------------------------
-- special instructions and functions
------------------------------------------------------------------------
__codeSize :: EvmAsm -> Int
__codeSize p = let (size, _, _) = genProgInfo (0, [], [])
                               . insts . execState p $ initState
               in size

