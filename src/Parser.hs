module Parser where

import Syntax
import Lexer
import ParserBase

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0).read

-- | Rewrite a list of tokens as a core program
syntax :: [Token] -> CoreProgram
syntax = fstP.pProgram
  where
    fstP ( ( prog, [] ) : others ) = prog
    fstP ( parse : others )        = fstP others
    fstP _                         = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar pVars (pLit "=") pExpr
  where
    mk_sc name args _ e = (name, args, e)

-- | TODO
pExpr :: Parser CoreExpr
pExpr = pThen EAp pExpr pAexpr
 `pAlt` pThen3 mk_inf pExpr pBinOp pExpr
 `pAlt` pThen4 mk_let ( (pLit "let") `pAlt` (pLit "letrec") ) pDefns (pLit "in") pExpr
 `pAlt` pThen4 mk_case (pLit "case")  pExpr (pLit "of") pAlters
 `pAlt` pThen4 mk_lam (pLit "\\")  pVars (pLit ".") pExpr
 `pAlt` pAexpr
  where
    mk_inf  = undefined
    mk_let  = undefined
    mk_letr = undefined
    mk_case = undefined
    mk_lam  = undefined

pAexpr :: Parser CoreExpr
pAexpr = (pVar `pApply` EVar)
  `pAlt` (pNum `pApply` ENum)


pBinOp :: Parser CoreExpr
pBinOp = undefined

pDefns :: Parser CoreExpr
pDefns = undefined

pAlters :: Parser CoreExpr
pAlters = undefined
