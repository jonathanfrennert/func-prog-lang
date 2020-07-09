{-|
Module      : Comp.TemplateBase
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Comp.TemplateBase (
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

-- | The statistics measure properties of program evaluation.
data TiStats = TiStats { step :: Int      -- ^ The number of evaluation steps
                       , pRedex :: Int    -- ^ The number of primitive reductions
                       , scRedex :: Int   -- ^ The number of supercombinator reductions
                       , depth :: Int     -- ^ The max stack depth
                       }
  deriving (Eq)

initialTiStat :: TiStats
initialTiStat = TiStats 0 0 0 0

-- | Update the number of steps after a reduction.
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (TiStats s p sc d) = TiStats (s + 1) p sc d

-- | Get the current number of steps.
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps (TiStats s _ _ _) = s

-- | Apply a statistics function to the 'TiStats' component of a 'TiState'.
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, sc_defs, stats)
  = (stack, dump, heap, sc_defs, f stats)
