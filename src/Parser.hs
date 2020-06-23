module Parser where

import Syntax
import Lexer
import ParserBase

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read

-- | Rewrite a list of tokens as a core program
syntax :: [Token] -> CoreProgram
syntax = undefined
