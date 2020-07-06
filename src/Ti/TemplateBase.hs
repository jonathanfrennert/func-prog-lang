{-|
Module      : Ti.TemplateBase
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Ti.TemplateBase (
  -- * Type
  Node (..),
  TiState,
  TiStack,
  TiDump (..),
  initialTiDump,
  TiHeap,
  TiGlobals,
  TiStats,
  initialTiStat,
  -- * Operations
  tiStatIncSteps,
  tiStatGetSteps,
  applyToStats
  ) where

import Lang.Syntax
import Utils.Heap
import Utils.Assoc

-- | Possible nodes in a heap.
data Node = NAp Addr Addr             -- ^ Application
  | NSupercomb Name [Name] CoreExpr   -- ^ Supercombinator
  | NNum Int                          -- ^ Number
    deriving (Show, Eq)

-- | Template instantiation state has a stack, dump, heap, globals and statistics.
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- | The stack spine is a list of addresses.
type TiStack = [Addr]

-- | The dump is currently unused.
data TiDump = DummyTiDump
  deriving (Show, Eq)

initialTiDump :: TiDump
initialTiDump = DummyTiDump

-- | The heap holds nodes.
type TiHeap = Heap Node

-- | The globals provides an address association for every supercombinator
-- name with the address of the heap node containing its definition.
type TiGlobals = ASSOC Name Addr

-- | The statistics measure the number of steps for evaluation.
type TiStats = Int

initialTiStat :: TiStats
initialTiStat = 0

-- | Update the statistics after a reduction.
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1

-- | Get the current statistics.
tiStatGetSteps :: TiStats -> TiStats
tiStatGetSteps = id

-- | Apply a statistics function to the 'TiStats' component of a 'TiState'.
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, sc_defs, stats)
  = (stack, dump, heap, sc_defs, f stats)
