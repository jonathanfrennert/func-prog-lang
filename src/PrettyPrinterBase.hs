module PrettyPrinterBase where

data Iseq = undefined

iNil :: Iseq  -- ^ The empty iseq
iStr :: String -> Iseq
iAppend :: Iseq -> Iseq -> Iseq
iIndent  :: Iseq -> Iseq

iNum :: Int -> Iseq
iNum = iStr.show

iSpace :: Iseq
iSpace = iStr " "

-- | New line with indentation
iNewline :: Iseq

-- | List seperator
iSep :: Iseq
iSep = iConcat [ iStr ";", iNewline ]

iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

-- | Interleave an iseq between each adjacent pair
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ []         = iNil
iInterleave _ [iseq]     = iseq
iInterleave sep (iq:iqs) = iConcat [iq, sep, iInterleave sep iqs]

-- | Turn an iseq into a string
iDisplay :: Iseq -> String
