module Utils.Heap where

import Utils.Assoc

-- | A heap holds the number of objects, unused addresses and
-- an association map of address to objects
data Heap a = Heap Int [Addr] (ASSOC Addr a)

type Addr = Int

hInitial :: Heap a
hInitial = Heap 0 [1..] []

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next : free) accs) n
  = (Heap (size + 1) free ( (next, n) : accs), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free accs) a n = Heap size free ((a, n) : remove accs a)

hFree :: Heap a -> Addr -> Heap a
hFree (Heap size free accs) a = Heap (size - 1) (a : free) (remove accs a)

hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ accs) a
  = aLookup accs a (error ("can’t find node " ++ showaddr a ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ accs) = [addr | (addr, node) <- accs]

hSize :: Heap a -> Int
hSize (Heap size _ _) = size

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
