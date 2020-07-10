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
  hGetSize,
  hGetAlloc,
  hGetUpdate,
  hGetFree,
  hNull,
  hIsnull,
  showaddr
  ) where

import Utils.Assoc

-- | A memory heap.
data Heap a = Heap { hStats :: (Int, Int, Int, Int)   -- ^ The statistics
                                                      -- include the number
                                                      -- of objects held,
                                                      -- number of 'hAlloc',
                                                      -- 'hUpdate', 'hFree',
                                                      -- operations performed
                   , hRems  :: [Addr]                 -- ^ Free addresses
                   , hAssoc :: (ASSOC Addr a) }       -- ^ Association map
                                                      -- of addresses and
                                                      -- objects

type Addr = Int

-- | An empty heap.
hInitial :: Heap a
hInitial = Heap (0,0,0,0) [1..] []

-- | Allocate an object in the heap and return the new heap with the address of
-- the object.
hAlloc :: Heap a
       -> a       -- ^ Object to be allocated
       -> (Heap a, Addr)
hAlloc (Heap (size, alloc, update, rems) (next : free) accs) n
  = ( Heap (size + 1, alloc + 1,  update, rems)
           free
           ( (next, n) : accs )
    , next )

-- | Update an address in the heap with a new object.
hUpdate :: Heap a
        -> Addr     -- ^ Address to be updated
        -> a        -- ^ Object linked to address
        -> Heap a
hUpdate (Heap (size, alloc, update, rems) free accs) a n
  = Heap (size, alloc, update + 1, rems)
         free
         ((a, n) : remove accs a)

-- | Free an address in the heap.
hFree :: Heap a
      -> Addr     -- ^ Address to be freed
      -> Heap a
hFree (Heap (size, alloc, update, rems) free accs) a
  = Heap (size - 1, alloc, update, rems + 1)
         (a : free)
         (remove accs a)

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
hAddresses (Heap _ _ accs) = aDomain accs

-- | Get the number of objects in the heap.
hGetSize :: Heap a -> Int
hGetSize (Heap (size, _, _ , _) _ _) = size

-- | Get the number of 'hAlloc' performed in the heap.
hGetAlloc :: Heap a -> Int
hGetAlloc (Heap (_, alloc, _, _) _ _) = alloc

-- | Get the number of 'hUpdate' performed in the heap.
hGetUpdate :: Heap a -> Int
hGetUpdate (Heap (_, _, update, _) _ _) = update

-- | Get the number of 'hFree' performed in the heap.
hGetFree :: Heap a -> Int
hGetFree (Heap (_, _, _, free) _ _) = free

-- | A unique address which points to no object.
hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull = (hNull ==)

-- | Show an address.
showaddr :: Addr -> String
showaddr a = "#" ++ show a
