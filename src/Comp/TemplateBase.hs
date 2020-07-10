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
  tiStatMaxDepth,
  tiStatGetDepth,
  tiStatRedex,
  tiStatGetPRedex,
  tiStatGetScRedex,
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
data TiStats = TiStats { steps :: Int     -- ^ The number of evaluation steps
                       , redex :: (Int, Int)    -- ^ First the number of
                                                -- primitive reductions and
                                                -- secondly the number of
                                                -- supercombinator reductions
                       , heap  :: Int   -- ^ The number of supercombinator reductions
                       , depth :: Int     -- ^ The max stack depth
                       }
  deriving (Eq)

initialTiStat :: TiStats
initialTiStat = TiStats 0 (0, 1) 0 0

-- | Update the number of steps after a reduction.
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (TiStats s rx h d) = TiStats (s + 1) rx h d

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps (TiStats s _ _ _) = s

-- | Update the max stack depth.
tiStatMaxDepth :: TiStack -> TiStats -> TiStats
tiStatMaxDepth stack stats@(TiStats s rx h d)
    | d' > d    = TiStats s rx h d'
    | otherwise = stats
    where
      d' = length stack

tiStatGetDepth :: TiStats -> Int
tiStatGetDepth (TiStats _ _ _ d) = d

-- | Update the number of reductions for a given type of reduction.
tiStatRedex :: TiStack -> TiHeap -> TiStats -> TiStats
tiStatRedex stack heap stats@(TiStats s (p, sc) h d)
  = identify $ hLookup heap $ head stack
  where
    identify (NAp _ _)          = TiStats s (p+1, sc) h d
    identify (NSupercomb _ _ _) = TiStats s (p , sc+1) h d
    identify _                  = stats

-- | Get the total number of primitive reductions.
tiStatGetPRedex :: TiStats -> Int
tiStatGetPRedex (TiStats _ (p, _) _ _) = p

-- | Get the total number of supercombinator reductions.
tiStatGetScRedex :: TiStats -> Int
tiStatGetScRedex (TiStats _ (_, sc) _ _) = sc

-- | Apply a statistics function to the 'TiStats' component of a 'TiState'.
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, sc_defs, stats)
  = (stack, dump, heap, sc_defs, f stats)
