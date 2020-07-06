{-|
Module      : Lang.Parser
License     : BSD-3
Maintainer  : jonathan.frennert@gmail.com
Stability   : experimental
-}
module Lang.Parser (
  -- * Main functions
  parse,
  syntax,
  -- * Standard syntax parsers
  pProgram,
  pSc,
  pExpr,
  pLet,
  pCase,
  pLambda,
  pAtomic,
  pConstr,
  pBracExpr,
  pDefns,
  pDefn,
  pAlters,
  pAlter,
  pArgs,
  pOneOrMoreWithSemicolon,
  -- * Infix and function application parsing
  -- $doc
  PartialExpr (..),
  -- ** Booleans
  pExpr1,
  pExpr1Snd,
  pExpr2,
  pExpr2Snd,
  -- ** Equivalence relations
  pExpr3,
  pExpr3Snd,
  -- ** Addition and subtraction
  pExpr4,
  pExpr4Snd,
  -- ** Multiplication and division
  pExpr5,
  pExpr5Snd,
  -- ** Function application
  pExpr6
  ) where

import Lang.ParserBase
import Lang.Syntax
import Lang.Lexer

-- $doc
-- Function application must be handled specially as it has left-recursive
-- grammar. To handle this we rewrite the grammar such that function application
-- is parsed like a series of atomic expressions, which are then applied
-- to one another from left to right. Infix operations are special as they have
-- identical structures, however the operations vary in precedence and
-- associativity. This is handled by recursively parsing infix expressions as
-- partial expressions ('PartialExpr'), where a partial expression can either
-- have no operator or an operator and a core expression. This makes it so
-- that when we parse infix operations we do not have to reparse an expression
-- if it turns out to have no infix operator following. Infix parsers always
-- check if an infix can be parsed by an operator of higher precedence.
-- Associativity is handled such that if an operator has no associativity, the
-- parser will only attempt to parse higher precedence operations in the
-- following expression, this way it is not possible for the operator to
-- be followed by the same operator.

-- | Attempt to parse a string into an abstract syntax tree.
parse :: String -> CoreProgram
parse = syntax.(flip clex $ 0)

-- | Attempt to parse tokens into an abstract syntax tree.
syntax :: [Token] -> CoreProgram
syntax = fstP.pProgram
  where
    fstP []                          = error "Program is unparseable!"
    fstP [( _, ( (line, tok) : _) )] = error $ "Syntax error at line " ++ show line
    fstP ( ( prog, [] ) : others )   = prog
    fstP ( parse : others )          = fstP others

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSemicolon pSc

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar pArgs (pLit "=") pExpr
  where
    mk_sc sc args _ rhs = (sc, args, rhs)

pExpr :: Parser CoreExpr
pExpr = pAlt pLet
      $ pAlt pCase
      $ pAlt pLambda pExpr1

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
pAtomic = pAlt pConstr
        $ pAlt pBracExpr
        $ pAlt (pVar `pApply` EVar) (pNum `pApply` ENum)

pConstr :: Parser CoreExpr
pConstr = pThen4 pick_constr (pLit "Cons") (pLit "{") pTagArity (pLit "}")
  where
    pick_constr  _ _ constr _  = constr
    pTagArity                  = pThen3 mk_constr pNum (pLit ",") pNum
    mk_constr tag _ arity      = EConstr tag arity

pBracExpr :: Parser CoreExpr
pBracExpr = pThen3 mk_brac (pLit "(") pExpr (pLit ")")
  where
    mk_brac _ e _ = e

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
    pTag                  = pThen3 mk_tag (pLit "<") pNum (pLit ">")
    mk_tag _ tag _        = tag

pArgs :: Parser [String]
pArgs = pZeroOrMore pVar

pOneOrMoreWithSemicolon :: Parser a -> Parser [a]
pOneOrMoreWithSemicolon p = pOneOrMoreWithSep p (pLit ";")

-- | Partial expressions are assembled depending on if they have an
-- application operation.
data PartialExpr = NoOp | FoundOp Name CoreExpr

-- | Assemble a partial expression into a core expession.
assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e NoOp             = e
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1Snd

pExpr1Snd :: Parser PartialExpr
pExpr1Snd = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2Snd

pExpr2Snd :: Parser PartialExpr
pExpr2Snd = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3Snd

pExpr3Snd :: Parser PartialExpr
pExpr3Snd = (pThen FoundOp pRelop pExpr4) `pAlt` (pEmpty NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4Snd

pExpr4Snd :: Parser PartialExpr
pExpr4Snd = pAlt (pThen FoundOp (pLit "+") pExpr4)
          $ pAlt (pThen FoundOp (pLit "-") pExpr5) (pEmpty NoOp)

pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5Snd

pExpr5Snd :: Parser PartialExpr
pExpr5Snd = pAlt (pThen FoundOp (pLit "*") pExpr5)
          $ pAlt (pThen FoundOp (pLit "/") pExpr6) (pEmpty NoOp)

pExpr6 :: Parser CoreExpr
pExpr6 = (pOneOrMore pAtomic) `pApply` mk_app_chain
  where
    mk_app_chain (fx:xs) = foldl EAp fx xs
