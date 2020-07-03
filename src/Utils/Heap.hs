{-|
Module      : Utils.Heap
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Utils.Heap (
  -- * Type
  Heap (..),
  Addr,
  -- * Operations
  hInitial,
  hAlloc,
  hUpdate,
  hFree,
  remove,
  hLookup,
  hAddresses,
  hSize,
  hNull,
  hIsnull,
  showaddr
  ) where

import Utils.Assoc

-- | A heap holds the number of objects, unused addresses and
-- an association map of address to objects
data Heap a = Heap Int [Addr] (ASSOC Addr a)

type Addr = Int

-- | An empty heap.
hInitial :: Heap a
hInitial = Heap 0 [1..] []

-- | Allocate an object in the heap and return the new heap with the address of
-- the object
hAlloc :: Heap a
       -> a       -- ^ Object to be allocated
       -> (Heap a, Addr)
hAlloc (Heap size (next : free) accs) n
  = (Heap (size + 1) free ( (next, n) : accs), next)

-- | Update an address in the heap with a new object.
hUpdate :: Heap a
        -> Addr     -- ^ Address to be updated
        -> a        -- ^ Object linked to address
        -> Heap a
hUpdate (Heap size free accs) a n = Heap size free ((a, n) : remove accs a)

-- | Free an address in the heap.
hFree :: Heap a
      -> Addr     -- ^ Address to be freed
      -> Heap a
hFree (Heap size free accs) a = Heap (size - 1) (a : free) (remove accs a)

-- | Attempt to remove an address from an address association map. If the
-- address is not in the association map an error is thrown.
remove :: ASSOC Addr a -> Addr -> ASSOC Addr a
remove [] a =
  error ("Attempt to update or free nonexistent address" ++ showaddr a)
remove ( (a', n) : accs) a
  | a == a'   = accs
  | otherwise = (a', n) : remove accs a

-- | Search for an address in the heap. If the object is there, it returns the
-- linked object, else an error is thrown.
hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ accs) a
  = aLookup accs a (error ("can’t find node " ++ showaddr a ++ " in heap"))

-- | Get all the addresses in the heap.
hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ accs) = [addr | (addr, node) <- accs]

-- | Get the number of objects in the heap.
hSize :: Heap a -> Int
hSize (Heap size _ _) = size

-- | A unique address which points to no object.
hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull = (hNull ==)

-- | Show an address.
showaddr :: Addr -> String
showaddr a = "#" ++ show a
