module Parser where

import Syntax
import Lexer

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s [] = []
pLit s ( (_, tok ) : toks )
    | s == tok  = [(tok, toks)]
    | otherwise = []

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read

-- | Rewrite a list of tokens as a core program.
syntax :: [Token] -> CoreProgram
syntax = undefined
