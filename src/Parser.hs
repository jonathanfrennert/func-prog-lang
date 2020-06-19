module Parser where

import Syntax
import Lexer

import Data.Char

type Parser a = [Token] -> [(a, [Token])]

pSat :: (String -> Bool) -> Parser String
pSat f ( (_, tok ) : toks )
  | f tok     = [(tok, toks)]
  | otherwise = []
pSat _ [] = []

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

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read

-- | Rewrite a list of tokens as a core program.
syntax :: [Token] -> CoreProgram
syntax = undefined
