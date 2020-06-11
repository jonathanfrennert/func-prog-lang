module PrettyPrintBase where

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq

-- | Turn an iseq into a string
iDisplay :: Iseq -> String
iDisplay iq = flatten [iq]

-- | Concatenates all iseqs in a list as a string.
flatten :: [Iseq] -> String
flatten [] = ""
flatten (INil : iqs) = flatten iqs
flatten (IStr s : iqs) = s ++ (flatten iqs)
flatten (IAppend iq1 iq2 : iqs) = flatten (iq1 : iq2 : iqs)

iNil :: Iseq  -- ^ The empty iseq
iNil = INil

iStr :: String -> Iseq
iStr str = IStr str

iAppend :: Iseq -> Iseq -> Iseq
iAppend INil iq = iq
iAppend iq INil = iq 
iAppend iq1 iq2 = IAppend iq1 iq2

iIndent  :: Iseq -> Iseq
iIndent iq = iq

-- | New line with indentation
iNewline :: Iseq
iNewline = IStr "\n"

iNum :: Int -> Iseq
iNum = iStr.show

iSpace :: Iseq
iSpace = iStr " "

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
