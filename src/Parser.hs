module Parser where

import Syntax
import Lexer
import ParserBase

parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0)

syntax :: [Token] -> CoreProgram
syntax = fstP.pProgram
  where
    fstP ( ( prog, [] ) : others ) = prog
    fstP ( parse : others )        = fstP others
    fstP _                         = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSemicolon pSc

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar pArgs (pLit "=") pExpr
  where
    mk_sc sc args _ rhs = (sc, args, rhs)

pExpr :: Parser CoreExpr
pExpr = pAlt pLet
      $ pAlt pCase
      $ pAlt pLambda pAtomic

pLet :: Parser CoreExpr
pLet = pThen4 mk_let
  ( (pLit "let") `pAlt` (pLit "letrec") )
  pDefns (pLit "in") pExpr
  where
    mk_let keyword defns _ e = ELet (keyword == "letrec") defns e

pCase :: Parser CoreExpr
pCase = pThen4 mk_case (pLit "case")  pExpr (pLit "of") pAlters
  where
    mk_case _ e _ alts = ECase e alts

pLambda :: Parser CoreExpr
pLambda = pThen4 mk_lam (pLit "\\")  pArgs (pLit ".") pExpr
  where
    mk_lam _ args _ e = ELam args e

pAtomic :: Parser CoreExpr
pAtomic = pAlt (pVar `pApply` EVar)
        $ pAlt (pNum `pApply` ENum)
        $ pAlt pConstr pBrackExpr

pConstr :: Parser CoreExpr
pConstr = pThen4 pick_constr (pLit "Cons") (pLit "{") pTagArity (pLit "}")
  where
    pick_constr  _ _ constr _  = constr
    pTagArity                  = pThen3 mk_constr pNum (pLit ",") pNum
    mk_constr tag _ arity      = EConstr tag arity

pBrackExpr :: Parser CoreExpr
pBrackExpr = pThen3 mk_brack (pLit "(") pExpr (pLit ")")
  where
    mk_brack _ e _ = e

pDefns :: Parser [CoreDefn]
pDefns = pOneOrMoreWithSemicolon pDefn

pDefn :: Parser CoreDefn
pDefn = pThen3 mk_defn pVar (pLit "=") pExpr
  where
    mk_defn var _ rhs = (var, rhs)

pAlters :: Parser [CoreAlt]
pAlters = pOneOrMoreWithSemicolon pAlter

pAlter :: Parser CoreAlt
pAlter = pThen4 mk_alt pTag pArgs (pLit "->") pExpr
  where
    mk_alt tag vars _ rhs = (tag, vars, rhs)
    pTag                = pThen3 mk_tag (pLit "<") pNum (pLit ">")
    mk_tag _ tag _      = tag

pArgs :: Parser [String]
pArgs = pZeroOrMore pVar

pOneOrMoreWithSemicolon :: Parser a -> Parser [a]
pOneOrMoreWithSemicolon p = pOneOrMoreWithSep p (pLit ";")
