
module Language.Evm (
    ) where

import           Control.Monad.State


------------------------------------------------------------------------
-- basic type and data
------------------------------------------------------------------------
type Evm a = State EvmState a
type EvmAsm = Evm ()

-- data EvmState = EvmState {
newtype EvmState = EvmState {
                  insts :: [EvmIr]
                } deriving Show

initState :: EvmState
initState = EvmState []

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


------------------------------------------------------------------------
-- asm utility
------------------------------------------------------------------------
makeAsm :: EvmIr -> EvmAsm
makeAsm op = do
    i <- gets insts
    modify $ \s -> s { insts = i ++ [op] }


------------------------------------------------------------------------
-- special instructions and functions
------------------------------------------------------------------------
-- pseudo instructions
_label :: String -> EvmAsm
_label x = makeAsm $ P_LABEL x

_dest :: String -> EvmAsm
_dest x = makeAsm $ P_JUMPDEST x

_raw :: Int -> EvmAsm
_raw x = makeAsm $ P_RAW x


-- macro instruction
_jump :: String -> EvmAsm
_jump x = makeAsm $ P_JUMP x

_push :: String -> EvmAsm
_push x = makeAsm $ P_PUSH x


-- built-in function
_codeSize :: EvmAsm -> Int
_codeSize p = let (size, _, _) = genProgInfo (0, [], [])
                               . insts . execState p $ initState
              in size

------------------------------------------------------------------------
-- definition of asm
------------------------------------------------------------------------
stop :: EvmAsm
stop = makeAsm STOP

add :: EvmAsm
add = makeAsm ADD

push1 :: Int -> EvmAsm
push1 x = makeAsm $ PUSH 1 x

push2 :: Int -> EvmAsm
push2 x = makeAsm $ PUSH 2 x

jump :: EvmAsm
jump = makeAsm JUMP


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
-- codemap :: EvmIr -> Either String String
type EvmCode = String

codemap :: EvmIr -> EvmCode
codemap ADD = "01"
codemap _   = "@@"

ir2code :: [EvmIr] -> EvmCode
ir2code []     = ""
ir2code (x:xs) = codemap x ++ ir2code xs
-- ir2code = foldr ((++) . codemap) ""

asm2code :: EvmAsm -> EvmCode
asm2code = ir2code . ir2ir . asm2ir

codegen :: EvmAsm -> EvmCode
codegen = asm2code



------------------------------------------------------------------------
-- example
------------------------------------------------------------------------
prog0 :: EvmAsm
prog0 = do
    push1 0x1
    push2 0x2

    _label "tag1"
    add
    _jump "target1"

    _dest "target1"
    add
    _jump "target2"

    _dest "target2"
    push1 (_codeSize prog1)
    _push "top1"
    stop


prog1 :: EvmAsm
prog1 = do
    _label "top1"
    stop
    _raw 0x7; _raw 0x8


-- composable
prog :: EvmAsm
prog = do
    prog0
    prog1


-- tmp test
z1 = asm2ir prog
z2 = ir2ir z1
z3 = ir2code z2
zd = debugShowIr prog

{-
zi = asm2ir prog
zt = genProgInfo (0, [], []) zi
(zcodeSize, _, zsymbolMap) = zt
zc = convSymbol zsymbolMap (P_JUMP "label1")
zr = resolveSymbols zsymbolMap zi
-}



