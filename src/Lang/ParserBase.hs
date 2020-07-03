{-|
Module      : Lang.ParserBase
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Lang.ParserBase (
  -- * Type
  Parser (..),
  -- * Basic parsers
  pSat,
  pLit,
  pVar,
  pRelop,
  pNum,
  -- * Special parsers
  pAlt,
  pApply,
  pThen,
  pThen3,
  pThen4,
  pEmpty,
  pZeroOrMore,
  pOneOrMore,
  pOneOrMoreWithSep,
  pFst
  ) where

import Lang.Syntax
import Lang.Lexer

import Data.Char

-- | A parser will parse a token or many tokens and return all possible
-- parsing alternatives.
type Parser a = [Token] -> [(a, [Token])]

-- | Parser only accepts tokens which fufill a certain property.
pSat :: (String -> Bool)  -- ^ Property checker
      -> Parser String
pSat _ [] = []
pSat f ( (_, tok ) : toks )
  | f tok     = [(tok, toks)]
  | otherwise = []

pLit :: String -> Parser String
pLit s = pSat (s ==)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar :: Parser String
pVar = pSat isVar
  where
    isVar s = s `notElem` keywords
            && ( isAlpha.head $ s )

relops :: [String]
relops = ["<", "<=", "==", "~=", ">=", ">"]

pRelop :: Parser String
pRelop = pSat (`elem` relops)

pNum :: Parser Int
pNum = pSat (and.map (isDigit)) `pApply` read

-- | Perform two different parses on the same token list and keep both results.
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

-- | Apply a function to parse results.
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [ (f v1, toks1) | (v1, toks1)  <- p toks  ]

-- | Create a parser that is the results of consecutively applying two parsers
-- and combining the respective results.
pThen :: (a -> b -> c)  -- ^ Combining function
      -> Parser a       -- ^ First parser
      -> Parser b       -- ^ Second parser
      -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                             , (v2, toks2) <- p2 toks1 ]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
  = [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks
                                , (v2, toks2) <- p2 toks1
                                , (v3, toks3) <- p3 toks2 ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks
  = [ (combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks
                                   , (v2, toks2) <- p2 toks1
                                   , (v3, toks3) <- p3 toks2
                                   , (v4, toks4) <- p4 toks3 ]

-- | The identity parser always succeeds. It does not consume any token, rather
-- it places a given value as the result.
pEmpty :: a         -- ^ Result
       -> Parser a
pEmpty res toks = [(res, toks)]

-- | Create a parser which applies a given parser zero or more times
-- and makes the result the list of successful parses.
pZeroOrMore :: Parser a   -- ^ Parser applied zero or more times
            -> Parser [a]
pZeroOrMore p = pFst $ (pOneOrMore p) `pAlt` (pEmpty [])

-- | Create a parser which applies a given parser one or more times
-- and makes the result the list of successful parses.
pOneOrMore :: Parser a    -- ^ Parser applied one or more times
           -> Parser [a]
pOneOrMore p = pFst $ pThen (:) p (pZeroOrMore p)

-- | Create a parser which applies a given parser one or more times
-- whilst ignoring separators.
pOneOrMoreWithSep :: Parser a   -- ^ Parser applied one or more times
                  -> Parser b   -- ^ Parser for ignored seperator
                  -> Parser [a]
pOneOrMoreWithSep p pSep = pFst $ pThen (:)  p (pOneOrMoreWithSep' p pSep)

-- | Used in 'pOneOrMoreWithSep' to remove the separators. Not intended
-- to be used by itself.
pOneOrMoreWithSep' :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep' p pSep = pFst $ (pThen snd' pSep ( pOneOrMoreWithSep p pSep ) )
  `pAlt` (pEmpty [])
  where snd' = curry snd

-- | Parser which only accepts the left-most parse result of the given parser.
pFst :: Parser a -> Parser a
pFst p = head'.p
  where
    head' (x:xs) = [x]
    head' x      = x
