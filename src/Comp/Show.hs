{-|
Module      : Comp.Show
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Comp.Show (
  -- * Main
  showResults,
  -- * Auxiliary
  showState,
  showStats,
  -- ** Stack
  showStack,
  showStkNode,
  showNode,
  showStkTree,
  showStkLeaf,
  showAddr,
  showFWAddr
  ) where

import Comp.TemplateBase
import Lang.PPrintBase
import Utils.Heap

-- | Format final state result.
showResults :: [TiState] -> String
showResults states =
  iDisplay.iConcat $ [ iNewline
                     , iStr " | EXECUTION"
                     , iNewline
                     , iLayn ( map showState states )
                     , showStats ( last states ) ]

-- | Shows the state's stack.
showState :: TiState -> Iseq
showState (stack, _, heap, _, _)
  = iConcat [ iInterleave iNewline [ showStack heap stack
                                   , showHeap heap
                                   , showSpine heap stack ]
            ]
  where
    showSpine heap stack = iConcat [ iNewline
                                   , iStr "Tree (Stk) ──┐"
                                   , iNewline
                                   , iStr "             "
                                   , iIndent (showStkTree heap stack (iStr "") (iStr "") True) ]

showHeap :: TiHeap -> Iseq
showHeap heap
  = iConcat [ iStr "Hp  ["
            , iIndent (iInterleave iNewline (map show_heap_item $ hAddresses heap))
            , iStr " ]" ]
  where
    show_heap_item addr = iConcat [showFWAddr addr, iStr ": "
                                  , showNode (hLookup heap addr) ]

-- | Show all the stack addresses and corresponding heap nodes.
showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
  = iConcat [ iStr "Stk ["
            , iIndent (iInterleave iNewline (map show_stack_item stack))
            , iStr " ]" ]
  where
    show_stack_item addr = iConcat [ showFWAddr addr, iStr ": "
                                   , showStkNode heap (hLookup heap addr) ]

-- | Show a stack address heap node.
showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr)
  = iConcat [ iStr "NAp ", showFWAddr fun_addr
            , iStr " "
            , showFWAddr arg_addr
            , iStr "("
            , showNode $ hLookup heap arg_addr
            , iStr ")" ]
showStkNode _ node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2)                 = iConcat [ iStr "NAp "
                                               , showAddr a1
                                               , iStr " "
                                               , showAddr a2 ]
showNode (NSupercomb name _ _)       = iStr ("NSupercomb " ++ name)
showNode (NNum n)                    = (iStr "NNum ") `iAppend` (iNum n)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

-- | Show an addresses with it contents.
showAddrNode :: TiHeap -> Addr -> Iseq
showAddrNode heap addr = iConcat [ showAddr addr
                                 , iStr " ("
                                 , showNode $ hLookup heap addr
                                 , iStr ")" ]

-- | Show a stack tree leaf.
showStkLeaf :: TiHeap     -- ^ Heap for the given address
            -> Addr       -- ^ Stack address
            -> Iseq       -- ^ Prefix
            -> Bool       -- ^ True if this is the tail, otherwise false
            -> Iseq
showStkLeaf heap addr prefix isTail = iConcat [ leaf_prefix
                                              , addr_format
                                              , iNewline]
  where
    leaf_prefix
      | isTail    = prefix `iAppend` (iStr "└── ")
      | otherwise = prefix `iAppend` (iStr "┌── ")
    addr_format
      | isTail    = showAddr addr
      | otherwise = showAddrNode heap addr

-- | Show the stack as a tree.
showStkTree :: TiHeap     -- ^ Heap for the given stack
            -> TiStack    -- ^ Stack
            -> Iseq       -- ^ Buffer
            -> Iseq       -- ^ Prefix
            -> Bool       -- ^ True if this is the tail, otherwise false
            -> Iseq
showStkTree _ [] def _ _                     = def
showStkTree heap [addr] buffer prefix isTail = treeify $ hLookup heap addr
  where
    treeify (NAp a1 a2) = iConcat [ buffer
                                  , showStkLeaf heap a2 right_prefix False
                                  , showStkLeaf heap addr prefix isTail
                                  , showStkLeaf heap a1 left_prefix True
                                  ]
      where
        right_prefix
          | isTail    = prefix `iAppend` (iStr "│   ")
          | otherwise = prefix `iAppend` (iStr "    ")
        left_prefix
          | isTail    = prefix `iAppend` (iStr "    ")
          | otherwise = prefix `iAppend` (iStr "|   ")
    treeify node        = iConcat [ buffer
                                  , prefix `iAppend` (iStr "└── ")
                                  , showAddrNode heap addr
                                  , iNewline]
showStkTree heap stack buffer prefix isTail = treeify $ hLookup heap current
  where
    current = last stack
    treeify (NAp a1 a2) = iConcat [ buffer
                                  , showStkLeaf heap a2 right_prefix False
                                  , showStkLeaf heap current prefix isTail
                                  , showStkTree heap (take (length stack - 1) stack) (iStr "") left_prefix True ]
      where
        right_prefix
          | isTail    = prefix `iAppend` (iStr "│   ")
          | otherwise = prefix `iAppend` (iStr "    ")
        left_prefix
          | isTail    = prefix `iAppend` (iStr "    ")
          | otherwise = prefix `iAppend` (iStr "|   ")

-- | Show a left padded address with width 4.
showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

-- | Show the evaluation statistics.
showStats :: TiState -> Iseq
showStats (_, _, heap, _, stats)
  = iConcat [ iNewline
            , iStr " | STATS"
            , iNewline
            , iInterleave iNewline [ iStr " - Total"
                                   , iStr "   * steps                        = "
                                     `iAppend` iNum (tiStatGetSteps stats)
                                   , iStr "   * primitive reductions         = "
                                     `iAppend` iNum (tiStatGetPRedex stats)
                                   , iStr "   * supercombinator reductions   = "
                                     `iAppend` iNum (tiStatGetScRedex stats)
                                   , iStr " - Heap"
                                   , iStr "   * size              = "
                                     `iAppend` iNum (hGetSize heap)
                                   , iStr "   * allocations       = "
                                     `iAppend` iNum (hGetAlloc heap)
                                   , iStr "   * updates           = "
                                     `iAppend` iNum (hGetUpdate heap)
                                   , iStr "   * addresses freed   = "
                                     `iAppend` iNum (hGetFree heap)
                                   , iStr " - Max stack depth = "
                                     `iAppend` iNum (tiStatGetDepth stats) ]
            , iNewline ]
