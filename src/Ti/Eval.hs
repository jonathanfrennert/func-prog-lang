{-|
Module      : Ti.Eval
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Ti.Eval (
  eval
  )where

import Ti.TemplateBase
import Utils.Heap

-- | Perform repeated state transitions until a final state is reached.
eval :: TiState -> [TiState]
eval state = state : rest_states
  where
    rest_states = if (tiFinal state)
                    then []
                    else (eval next_state)
    next_state = doAdmin (step state)

-- | Update state statistics.
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

-- | Step one state forward.
step :: TiState -> TiState
step = undefined

-- | Check if a given template state is final.
tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats)
  = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

-- | Check if a given node is a valid final result.
isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False
