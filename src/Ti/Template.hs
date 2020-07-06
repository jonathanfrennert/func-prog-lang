{-|
Module      : Ti.Template
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Ti.Template (
  runProg,
  compile,
  showResults
  ) where

import Ti.Eval
import Ti.TemplateBase
import Lang.Syntax
import Lang.Parser
import Lang.StdPrelude
import Utils.Heap
import Utils.Assoc
import Utils.Data

-- | Run a program and return the results.
runProg :: String   -- ^ Filename containing program
        -> String
runProg = showResults.eval.compile.parse

-- | produces the initial state of the template instantiation machine.
compile :: CoreProgram -> TiState
compile prog =
  (initial_stack, initialTiDump, initial_heap, globals, initialTiStat)
  where
    sc_defs = prog ++ preludeDefs

    (initial_heap, globals) = buildInitialHeap sc_defs

    initial_stack   = [address_of_main]
    address_of_main = aLookup globals "main" (error "main is not defined")

-- | Create the heap and globals from a  core program.
buildInitialHeap :: CoreProgram -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccuml allocateSc hInitial

-- | Allocate a supercombinator to a heap.
allocateSc :: TiHeap                    -- ^ The heap
           -> CoreScDefn                -- ^ Supercombinator to be allocated
           -> (TiHeap, (Name, Addr))    -- ^ New heap and address association
allocateSc heap (name, args, body) = ( heap', (name, addr) )
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

-- | Format final state result.
showResults :: [TiState] -> String
showResults = undefined
