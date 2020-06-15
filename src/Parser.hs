module Parser where

import Syntax
import Lexer

-- | Rewrite a list of tokens as a core program.
syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read
