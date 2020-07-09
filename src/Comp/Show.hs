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
  showAddr,
  showFWAddr
  ) where

import Comp.TemplateBase
import Lang.PPrintBase
import Utils.Heap

-- | Format final state result.
showResults :: [TiState] -> String
showResults states =
  iDisplay.iConcat $ [ iLayn ( map showState states )
                     , showStats ( last states ) ]

-- | Shows the state's stack.
showState :: TiState -> Iseq
showState (stack, _, heap, _, _)
  = iConcat [ iInterleave iNewline [ showStack heap stack, showHeap heap ]
            ]

showHeap :: TiHeap -> Iseq
showHeap heap
  = iConcat [ iStr "Hp  ["
            , iIndent (iInterleave iNewline (map show_heap_item $ hAddresses heap))
            , iStr " ]" ]
  where
    show_heap_item addr = iConcat [showFWAddr addr, iStr ": "
                                  , showNode (hLookup heap addr) ]

-- | Show all the stack adresses and corresponding heap nodes.
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
            , iStr " ("
            , showNode (hLookup heap arg_addr), iStr ")" ]
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

-- | Show a left padded address with width 4.
showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

-- | Show the total number of evaluation steps.
showStats :: TiState -> Iseq
showStats (_, _, _, _, stats)
  = iConcat [ iNewline, iNewline, iStr "Total number of steps = "
            , iNum (tiStatGetSteps stats) ]
