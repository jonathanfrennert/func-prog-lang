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
  indStep,
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
import Utils.Data


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
doAdmin state@(stack, dump, heap, globals, stats)
  = applyToStats (tiStatRedex stack heap)
  . applyToStats (tiStatMaxDepth stack)
  . applyToStats tiStatIncSteps $ state

-- | Check if a given template state is final.
tiFinal :: TiState -> Bool
tiFinal ([sole_addr], _, heap, _, _)
                                         = isDataNode (hLookup heap sole_addr)
tiFinal ([], _, _, _, _)                 = error "Empty stack!"
tiFinal state                            = False

-- | Check if a given node is a valid final result.
isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode node     = False

-- | Step one state forward.
step :: TiState -> TiState
step state@(stack, _, heap, _, _) = dispatch $ hLookup heap $ head stack
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = appStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body
    dispatch (NInd a)                  = indStep state a

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
-- addresses in the stack with the address to the result. This is pointed to
-- with an indirection node from the root of the redex in the heap.
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
  | length stack - 1 < length arg_names = error $ concat ["Supercombinator ", sc_name, " has too few arguments applied!"]
  | otherwise                           = (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = drop (length arg_names) stack
    new_heap  = hUpdate heap' (head new_stack) (NInd result_addr)

    (heap', result_addr) = instantiate body heap (arg_bindings ++ globals)
    arg_bindings         = zip arg_names (getArgs heap stack)

-- | Get heap argument addresses from stack node adresses.
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack)
  = map get_arg stack
  where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

-- | The indirection rule replaces the root of the redex in the stack with the
-- result address.
indStep :: TiState -> Addr -> TiState
indStep (a : stack, dump, heap, globals, stats) a'
  = (a' : stack, dump, heap, globals, stats)

-- | Creates an instance of an expression and updates the result node.
instantiateAndUpdate :: CoreExpr        -- ^ Body of supercombinator
                     -> Addr            -- ^ Address of node to update
                     -> TiHeap          -- ^ Heap before instantiation
                     -> ASSOC Name Addr -- ^ Association of names to addresses
                     -> TiHeap
instantiateAndUpdate (EVar v) upd_addr heap env
  = hUpdate heap upd_addr (NInd var_addr)
    where
      var_addr = aLookup env v (error ("Undefined name " ++ show v))
instantiateAndUpdate (EAp e1 e2) upd_addr heap env
  = hUpdate heap2 upd_addr (NAp a1 a2)
    where
      (heap1, a1) = instantiate e1 heap env
      (heap2, a2) = instantiate e2 heap1 env

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

-- | Instantiate let(rec)s.
instLet :: Bool -> [CoreDefn] -> CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instLet isRec defs body heap env = instantiate body new_heap new_env
  where
    (new_heap, addrs) = mapAccuml (\x y -> instantiate y x pass_env) heap (aRange defs)
    pass_env
      | isRec     = new_env
      | otherwise = env
    new_env           = def_bindings ++ env
    def_bindings      = zip (aDomain defs) addrs
