module Parser where

import Syntax

import Data.Char
import Data.List

type Token = (Int, String)

twoCharOps :: [String]
twoCharOps = ["==", "˜=", ">=", "<=", "->"]

-- | Lexical analysis of a program
clex :: String    -- ^ Unprocessed program
     -> Int       -- ^ Current line number
     -> [Token]
clex [] _ = []
clex ('-' : '-' : cs) n = clex (dropWhile (/= '\n') cs) n
clex (c1 : c2 : cs) n
  | [c1, c2] `elem` twoCharOps = (n, [c1, c2])  : clex cs n
clex ('\n' : cs) n = clex cs (n + 1)
clex (c:cs) n
  | isSpace c = clex cs n

  | isDigit c =
    let
      num_token = c : takeWhile isDigit cs
      rest_cs   = dropWhile isDigit cs
    in
      (n, num_token) : clex rest_cs n

  | isAlpha c =
    let
      var_tok = c : takeWhile isIdChar cs
      rest_cs = dropWhile isIdChar cs
    in
      (n, var_tok) : clex rest_cs n

  | otherwise = (n, [c]) : clex cs n

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

-- | Rewrite a list of tokens as a core program.
syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read
