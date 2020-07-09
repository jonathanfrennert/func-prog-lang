{-|
Module      : Comp.Eval
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Comp.Eval (
  eval,
  res,
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

import Comp.TemplateBase
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

-- | Get the final state result node.
res :: TiState -> Node
res state@(stack, _, heap, _, _)
  | tiFinal state = hLookup heap $ head stack
  | otherwise     = error "State is not final!"

-- | Update state statistics.
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

-- | Check if a given template state is final.
tiFinal :: TiState -> Bool
tiFinal ([sole_addr], _, heap, _, _)
                                         = isDataNode (hLookup heap sole_addr)
tiFinal ([], _, _, _, _) = error "Empty stack!"
tiFinal state                            = False

-- | Check if a given node is a valid final result.
isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode node     = False

-- | Step one state forward.
step :: TiState -> TiState
step state = dispatch $ hLookup heap $ head stack
  where
    (stack, _, heap, _, _) = state

    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = appStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

-- | A number step throws an error as it should never occur.
numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as a function!"

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
getArgs heap (_ : stack)
  = map get_arg stack
  where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

-- | Create an instance of an expression in the heap.
instantiate :: CoreExpr           -- ^ Body of supercombinator
            -> TiHeap             -- ^ Heap before instantiation
            -> ASSOC Name Addr    -- ^ Association of names to addresses
            -> (TiHeap, Addr)     -- ^ Heap after instantiation, and address of root of instance
instantiate (EVar v) heap env               =
  (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (ENum n) heap _                 = hAlloc heap (NNum n)
instantiate (EConstr tag arity) heap env    = instConstr tag arity heap env
instantiate (EAp e1 e2) heap env            = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (ELet isrec defs body) heap env = instLet isrec defs body heap env
instantiate (ECase _ _) _ _                 =
  error "Can’t instantiate case expressions!"

-- | Instantiate a constructor. Currently unused.
instConstr :: Int -> Int -> TiHeap -> ASSOC Name Addr -> a
instConstr _ _ _ _
  = error "Can’t instantiate constructors yet!"

-- | Instantiate let(rec)s. Currently unused.
instLet :: Bool -> [CoreDefn] -> CoreExpr -> TiHeap -> ASSOC Name Addr -> a
instLet _ _ _ _ _
  = error "Can’t instantiate let(rec)s yet!"
