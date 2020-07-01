module Utils.Assoc where

type ASSOC a b = [(a,b)]

aLookup :: Eq a => ASSOC a b -> a -> b -> b
aLookup [] k' def = def
aLookup ( (k, v) : bs) k' def
  | k == k'   = v
  | otherwise = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain ls = [key | (key, val) <- ls]

aRange :: ASSOC a b -> [b]
aRange ls = [val | (key, val) <- ls]

aEmpty :: ASSOC a b
aEmpty = []
