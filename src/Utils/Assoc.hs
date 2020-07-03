{-|
Module      : Utils.Assoc
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Utils.Assoc (
  -- * Type
  ASSOC,
  -- * Operations
  aEmpty,
  aLookup,
  aDomain,
  aRange
  ) where

-- | An association map is composed of a list of tuples which link the first
-- element to the second element.
type ASSOC a b = [(a,b)]

-- | An empty association map.
aEmpty :: ASSOC a b
aEmpty = []

-- | Lookup a first element in the list and return its corresponding second
-- element. If no value is found, a default value is returned.
aLookup :: Eq a => ASSOC a b
        -> a                    -- ^ Search key
        -> b                    -- ^ Default value
        -> b
aLookup [] k' def = def
aLookup ( (k, v) : bs) k' def
  | k == k'   = v
  | otherwise = aLookup bs k' def

-- | Get the domain of an association map, i.e., all the first elements.
aDomain :: ASSOC a b -> [a]
aDomain ls = [key | (key, val) <- ls]

-- | Get the range of an association map, i.e., all the second elements.
aRange :: ASSOC a b -> [b]
aRange ls = [val | (key, val) <- ls]
