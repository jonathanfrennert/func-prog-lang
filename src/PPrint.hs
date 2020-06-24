module PPrint where

import Syntax
import PPrintBase

-- | Operator precedence
appPrec, multPrec, divPrec, addPrec, subPrec, eqPrec, andPrec, orPrec, noPrec :: Int
appPrec   = 6
multPrec  = 5
divPrec   = 5
addPrec   = 4
subPrec   = 4
eqPrec    = 3
andPrec   = 2
orPrec    = 1
noPrec    = 0

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave (iAppend (iStr " ;") iNewline) (map pprSc prog)

-- | Pretty Print supercombinator definition
pprSc :: CoreScDefn -> Iseq
pprSc (name, args, body) = iConcat [ iStr name, iSpace, pprArgs args
                                   , iStr " = ", iIndent $ pprExpr body noPrec]

pprExpr :: CoreExpr -> Int -> Iseq
pprExpr (EVar v) _ = iStr v
pprExpr (ENum n) _ = iNum n

pprExpr (EAp (EAp (EVar "*") e1) e2) n
  | n > multPrec  = iConcat [ iBracL, prod, iBracR ]
  | otherwise     = prod
  where
    prod = iConcat [pprExpr e1 multPrec, iStr " * ", pprExpr e2 multPrec]

pprExpr (EAp (EAp (EVar "/") e1) e2) n
  | n > divPrec  = iConcat [ iBracL, quot', iBracR ]
  | otherwise     = quot'
  where
    quot' = iConcat [pprExpr e1 divPrec, iStr " / ", pprExpr e2 divPrec]

pprExpr (EAp (EAp (EVar "+") e1) e2) n
  | n > addPrec = iConcat [ iBracL, add, iBracR ]
  | otherwise   = add
  where
    add = iConcat [pprExpr e1 addPrec, iStr " + ", pprExpr e2 addPrec]

pprExpr (EAp (EAp (EVar "-") e1) e2) n
  | n > subPrec = iConcat [ iBracL, sub, iBracR ]
  | otherwise   = sub
  where
    sub = iConcat [pprExpr e1 subPrec, iStr " - ", pprExpr e2 subPrec]

pprExpr (EAp (EAp (EVar "==") e1) e2) n
  | n > eqPrec  = iConcat [iBracL, eq, iBracR]
  | otherwise   = eq
  where
    eq = iConcat [pprExpr e1 eqPrec, iStr " == ", pprExpr e2 eqPrec]

pprExpr (EAp (EAp (EVar "~=") e1) e2) n
  | n > eqPrec  = iConcat [iBracL, eq, iBracR]
  | otherwise   = eq
  where
    eq = iConcat [pprExpr e1 eqPrec, iStr " ~= ", pprExpr e2 eqPrec]

pprExpr (EAp (EAp (EVar ">") e1) e2) n
  | n > eqPrec  = iConcat [iBracL, eq, iBracR]
  | otherwise   = eq
  where
    eq = iConcat [pprExpr e1 eqPrec, iStr " > ", pprExpr e2 eqPrec]

pprExpr (EAp (EAp (EVar ">=") e1) e2) n
  | n > eqPrec  = iConcat [iBracL, eq, iBracR]
  | otherwise   = eq
  where
    eq = iConcat [pprExpr e1 eqPrec, iStr " >= ", pprExpr e2 eqPrec]

pprExpr (EAp (EAp (EVar "<") e1) e2) n
  | n > eqPrec  = iConcat [iBracL, eq, iBracR]
  | otherwise   = eq
  where
    eq = iConcat [pprExpr e1 eqPrec, iStr " < ", pprExpr e2 eqPrec]

pprExpr (EAp (EAp (EVar "<=") e1) e2) n
  | n > eqPrec  = iConcat [iBracL, eq, iBracR]
  | otherwise   = eq
  where
    eq = iConcat [pprExpr e1 eqPrec, iStr " <= ", pprExpr e2 eqPrec]

pprExpr (EAp (EAp (EVar "&") e1) e2) n
  | n > andPrec  = iConcat [iBracL, and', iBracR]
  | otherwise   = and'
  where
    and' = iConcat [pprExpr e1 andPrec, iStr " & ", pprExpr e2 andPrec]

pprExpr (EAp (EAp (EVar "|") e1) e2) n
  | n > orPrec  = iConcat [iBracL, and', iBracR]
  | otherwise   = and'
  where
    and' = iConcat [pprExpr e1 orPrec, iStr " | ", pprExpr e2 orPrec]

pprExpr (EAp e1 e2) n
  | n == appPrec  = iConcat [iBracL, app, iBracR]
  | otherwise     = app
  where
    app = iConcat [pprExpr e1 noPrec, iSpace, pprExpr e2 appPrec]

pprExpr (ELet isrec defns expr) _ =
  iConcat [ iStr keyword, iNewline
          , iSpace, iIndent (pprDefns defns), iNewline
          ,  iStr "in ", pprExpr expr noPrec]
  where
    keyword
      | isrec     = "letrec"
      | otherwise = "let"

pprExpr (ECase e alts) _          =
  iConcat [ iStr "case ", pprExpr e noPrec, iStr " of", iNewline
          , iStr "  " , iIndent (iInterleave iSep (map pprAlt alts))]

pprExpr (ELam args body) _        =
  iConcat [ iStr "(\\", pprArgs args, iStr ". ", iIndent $ pprExpr body noPrec
          , iBracR ]

-- | Pretty print case alternatives
pprAlt :: CoreAlt -> Iseq
pprAlt (tag, args, rhs) = iConcat [  iStr "<", iNum tag, iStr "> "
                                  ,  pprArgs args,  iStr " -> "
                                  ,  iIndent $ pprExpr rhs noPrec]

pprDefns :: [CoreDefn] -> Iseq
pprDefns defns = iInterleave iSep (map pprDefn defns)

pprDefn :: CoreDefn -> Iseq
pprDefn (name, e) = iConcat [ iStr name, iStr " = "
                            , iIndent (pprExpr e noPrec) ]

pprArgs :: [Name] -> Iseq
pprArgs =  iInterleave iSpace.map iStr
