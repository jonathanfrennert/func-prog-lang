module Parser where

import Syntax
import Lexer

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s [] = []
pLit s ( (_, tok ) : toks )
    | s == tok  = [(tok, toks)]
    | otherwise = []

pVar :: Parser String
pVar [] = []
pVar [ (_ , tok@(c:cs) )]
  | isVarChar c = [(tok , [])]
  | otherwise   = []
pVar ( (_, tok@(c:cs) ) : toks )
  | isVarChar c = [(tok , toks)]
  | otherwise   = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                             , (v2, toks2) <- p2 toks1]

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read

-- | Rewrite a list of tokens as a core program.
syntax :: [Token] -> CoreProgram
syntax = undefined
