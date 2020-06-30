module Utils where

-- | Apply a function to an accumulator and a list of elements
-- to accumulate elements and return a result list of the elements
mapAccuml :: (a -> b -> (a, c))   -- ^ Function of accumulator and element which
                                  -- returns new accumulator and result element
          -> a                    -- ^ Accumulator
          -> [b]                  -- ^ Input list
          -> (a, [c])
mapAccuml f acc []     = (acc, [])
mapAccuml f acc [x]   = (acc', [y])
  where
    (acc', y) = f acc x
mapAccuml f acc (x:xs) = (acc2, y:ys)
  where
    (acc1, y)  = f acc x
    (acc2, ys) = mapAccuml f acc1 xs
