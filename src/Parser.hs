module Parser where

import Syntax

import Data.Char

type Token = String

-- | Lexical analysis
clex :: String -> [Token]
clex [] = []
clex (c:cs)
  | isSpace c = clex cs

  | isDigit c =
    let
      num_token = c : takeWhile isDigit cs
      rest_cs   = dropWhile isDigit cs
    in
      num_token : clex rest_cs

  | isAlpha c =
    let
      var_tok = c : takeWhile isIdChar cs
      rest_cs = dropWhile isIdChar cs
    in
      var_tok : clex rest_cs

  | otherwise = [c] : clex cs

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

-- | Rewrite a list of tokens as a core program.
syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax.clex.read
