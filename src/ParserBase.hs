module ParserBase where

import Syntax
import Lexer

import Data.Char

type Parser a = [Token] -> [(a, [Token])]

pSat :: (String -> Bool) -> Parser String
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

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
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

-- | Identity parser
pEmpty :: a -> Parser a
pEmpty tok toks = [(tok, toks)]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p pSep = pThen (:)  p (pOneOrMoreWithSep' p pSep)

pOneOrMoreWithSep' :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep' p pSep = (pThen snd' pSep ( pOneOrMoreWithSep p pSep ) )
  `pAlt` (pEmpty [])
  where snd' = curry snd

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [ (f v1, toks1) | (v1, toks1)  <- p toks  ]

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read

-- | Rewrite a list of tokens as a core program
syntax :: [Token] -> CoreProgram
syntax = undefined
