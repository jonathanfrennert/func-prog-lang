module Heap where

import Assoc

-- | Template instantiation heap.
type Heap a = ( Int           -- ^ Number of objects
              , [Addr]        -- ^ Unused Addresses
              , ASSOC Addr a  -- ^ Association map of address to objects.
              )

type Addr = Int

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, ( next : free ), accs) n
  = ( (size + 1, free, ( next, n ) : accs), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, accs) a n = (size, free, (a, n) : remove accs a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, accs) a = (size - 1, a : free, remove accs a)

hLookup :: Heap a -> Addr -> a
hLookup (_, _, accs) a
  = aLookup accs a (error ("can’t find node " ++ showaddr a ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, accs) = [addr | (addr, node) <- accs]

hSize :: Heap a -> Int
hSize (size, _, _) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull = (hNull ==)

showaddr :: Addr -> [Char]
showaddr a = "#" ++ show a

remove :: [(Int,a)] -> Int -> [(Int,a)]
remove [] a =
  error ("Attempt to update or free nonexistent address" ++ showaddr a)
remove ( (a', n) : accs) a
  | a == a'   = accs
  | otherwise = (a', n) : remove accs a
