module TemplateBase where

import Syntax
import Heap
import Assoc

-- | Possible nodes in a heap
data Node = NAp Addr Addr             -- ^ Application
  | NSupercomb Name [Name] CoreExpr   -- ^ Supercombinator
  | NNum Int                          -- ^ A number
    deriving (Show, Eq)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

-- | Currently unused
data TiDump = DummyTiDump
  deriving (Show, Eq)

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

type TiGlobals = ASSOC Name Addr

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1

tiStatGetSteps :: TiStats -> TiStats
tiStatGetSteps = id

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, sc_defs, stats)
  = (stack, dump, heap, sc_defs, f stats)
