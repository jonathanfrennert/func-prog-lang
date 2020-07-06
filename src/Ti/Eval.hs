{-|
Module      : Ti.Eval
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Ti.Eval (
  eval,
  -- ** Step forward
  step,
  numStep,
  appStep,
  scStep,
  -- ** Instantiation
  instantiate,
  instConstr,
  instLet
  ) where

import Ti.TemplateBase
import Lang.Syntax
import Utils.Heap
import Utils.Assoc

-- | Perform repeated state transitions until a final state is reached.
eval :: TiState -> [TiState]
eval state = state : rest_states
  where
    rest_states = if (tiFinal state)
                    then []
                    else (eval next_state)
    next_state  = doAdmin (step state)

-- | Update state statistics.
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

-- | Check if a given template state is final.
tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats)
                                         = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state                            = False

-- | Check if a given node is a valid final result.
isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node     = False

-- | Step one state forward.
step :: TiState -> TiState
step state = dispatch $ hLookup heap $ head stack
  where
    (stack, dump, heap, globals, stats) = state

    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = appStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

-- | A number step throws an error as it should never occur.
numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

-- | The application rule places the applied function address at the head of
-- the stack.
appStep :: TiState -> Addr -> Addr -> TiState
appStep (stack, dump, heap, globals, stats) a1 a2
  = (a1 : stack, dump, heap, globals, stats)

-- | The supercombinator rule instantiates the supercombinator definition with
-- arguments whose heap node address is in the stack, and replaces those
-- addresses in the stack with the address to the supercombinator instantiation.
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
  = (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = result_addr : (drop (length arg_names+1) stack)

    (new_heap, result_addr) = instantiate body heap env
    env                     = arg_bindings ++ globals
    arg_bindings            = zip arg_names (getArgs heap stack)

-- | Get heap argument addresses from stack node adresses.
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack)
  = map get_arg stack
  where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

-- | Create an instance of an expression in the heap.
instantiate :: CoreExpr           -- ^ Body of supercombinator
            -> TiHeap             -- ^ Heap before instantiation
            -> ASSOC Name Addr    -- ^ Association of names to addresses
            -> (TiHeap, Addr)     -- ^ Heap after instantiation, and address of root of instance
instantiate (EVar v) heap env               =
  (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (ENum n) heap env               = hAlloc heap (NNum n)
instantiate (EConstr tag arity) heap env    = instConstr tag arity heap env
instantiate (EAp e1 e2) heap env            = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (ELet isrec defs body) heap env = instLet isrec defs body heap env
instantiate (ECase e alts) heap env         =
  error "Can’t instantiate case expressions!"

-- | Instantiate a constructor. Currently unused.
instConstr :: Int -> Int -> TiHeap -> ASSOC Name Addr -> a
instConstr tag arity heap env
  = error "Can’t instantiate constructors yet!"

-- | Instantiate let(rec)s. Currently unused.
instLet :: Bool -> [CoreDefn] -> CoreExpr -> TiHeap -> ASSOC Name Addr -> a
instLet isrec defs body heap env
  = error "Can’t instantiate let(rec)s yet!"
