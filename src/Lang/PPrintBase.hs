{-|
Module      : Lang.PPrintBase
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Lang.PPrintBase (
  -- * Type
  Iseq (..),
  -- * To String
  iDisplay,
  flatten,
  -- * To Iseq
  iNil,
  iStr,
  iAppend,
  iIndent,
  iNewline,
  iNum,
  iSpace,
  space,
  iBracL,
  iBracR,
  iSep,
  iFWNum,
  iConcat,
  iLayn,
  iInterleave,
  ) where

-- | We use this abstract data type to seperate the interface of pretty print
-- from its implementation. A benefit of this is that we can reduce the pretty
-- print from quadratic time to linear time by minimising the use of concat
-- ('++') in the implementation.
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

-- | Turn an iseq into a string.
iDisplay :: Iseq -> String
iDisplay iq = flatten 0 [(iq, 0)]

-- | Concatenates a list of iseqs as a string, formatting the string depending
-- on the given 'Iseq' type.
flatten :: Int              -- ^ Current column; 0 for first column
        -> [(Iseq, Int)]    -- ^ List to format
        -> String
flatten _ []                                   = ""
flatten col ( (INil, _) : iqs)                 = flatten col iqs
flatten col ( (IStr s, _) : iqs)               = s ++ (flatten (col + length s)  iqs)
flatten col ( (IAppend iq1 iq2, indent) : iqs) = flatten col ( (iq1, indent) : (iq2, indent) : iqs)
flatten col ( (IIndent iq, _) : iqs)           = flatten col ( (iq, col) : iqs)
flatten col ( (INewline, indent) : iqs)        = '\n' : space indent ++ (flatten indent iqs)

-- | The empty iseq.
iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr str
  | length str == length firstLine = IStr str
  | otherwise                      = iInterleave INewline (map iStr $ lines str)
  where
    firstLine = takeWhile (/= '\n') str

iAppend :: Iseq -> Iseq -> Iseq
iAppend INil iq = iq
iAppend iq INil = iq
iAppend iq1 iq2 = IAppend iq1 iq2

iIndent  :: Iseq -> Iseq
iIndent iq = IIndent iq

iNewline :: Iseq
iNewline = INewline

iNum :: Int -> Iseq
iNum = iStr.show

iSpace :: Iseq
iSpace = iStr " "

-- | Fill up space
space :: Int      -- ^ Number of spaces
      -> String
space n = take n (repeat ' ')

-- | Left paranthesis.
iBracL :: Iseq
iBracL = iStr "("

-- | Right paranthesis.
iBracR :: Iseq
iBracR = iStr ")"

-- | List item seperator.
iSep :: Iseq
iSep = iConcat [ iStr ";", iNewline ]

-- | Left-padded numbers.
iFWNum :: Int   -- ^ Line width
       -> Int   -- ^ Number
       -> Iseq
iFWNum width n = iStr (space (width - length digits) ++ digits)
  where
    digits = show n

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

-- | Numbered list.
iLayn :: [Iseq] -> Iseq
iLayn iqs = iConcat (map lay_item (zip [1..] iqs))
  where
    lay_item (n, iq) =
      iConcat [ iFWNum 4 n, iStr ") ", iIndent iq, iNewline ]

-- | Interleave an item between each adjacent pair.
iInterleave :: Iseq     -- ^ The item interleaved
            -> [Iseq]   -- ^ The list to be interleaved
            -> Iseq
iInterleave _ []         = iNil
iInterleave _ [iseq]     = iseq
iInterleave sep (iq:iqs) = iConcat [iq, sep, iInterleave sep iqs]
