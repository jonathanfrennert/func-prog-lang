{-|
Module      : Ti.Template
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Ti.Template where

import Ti.TemplateBase
import Lang.Syntax
import Lang.Parser
import Lang.StdPrelude
import Utils.Heap
import Utils.Assoc
import Utils.Data

runProg :: String -> String
runProg = showResults.eval.compile.parse

-- | produces the initial state of the template instantiation machine
compile :: CoreProgram -> TiState
compile prog =
  (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
  where
    sc_defs = prog ++ preludeDefs ++ extraPreludeDefs

    (initial_heap, globals) = buildInitialHeap sc_defs

    initial_stack = [address_of_main]
    address_of_main = aLookup globals "main" (error "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

buildInitialHeap :: CoreProgram -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccuml allocateSc hInitial

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = ( heap', (name, addr) )
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

-- | Perform repeated state transitions until a final state is reached
eval :: TiState -> [TiState]
eval = undefined

showResults :: [TiState] -> String
showResults = undefined
