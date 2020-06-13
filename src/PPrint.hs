module PPrint where

import Syntax
import PPrintType

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave (iAppend (iStr " ;") iNewline) (map pprSc prog)

pprSc :: CoreScDefn -> Iseq
pprSc (name, args, body) = iConcat [ iStr name, iSpace, pprArgs args
                                   , iStr " = ", iIndent $ pprExpr body 0]

appPrec, multPrec, divPrec, addPrec, minPrec, eqPrec, andPrec, orPrec :: Int
appPrec   = 6
multPrec  = 5
divPrec   = 5
addPrec   = 4
minPrec   = 4
eqPrec    = 3
andPrec   = 2
orPrec    = 1

pprExpr :: CoreExpr -> Int -> Iseq
pprExpr (EVar v) _                = iStr v
pprExpr (ENum n) _                = iNum n

pprExpr (EAp (EAp (EVar "*") e1) e2) n
  | n > multPrec  = iConcat [iStr "(", prod, iStr ")"]
  | otherwise     = prod
  where
    prod = iConcat [pprExpr e1 multPrec, iStr " * ", pprExpr e2 multPrec]

pprExpr (EAp (EAp (EVar "+") e1) e2) n
  | n > addPrec = iConcat [iStr "(", add, iStr ")"]
  | otherwise   = add
  where
    add = iConcat [pprExpr e1 addPrec, iStr " + ", pprExpr e2 addPrec]

pprExpr (EAp (EAp (EVar ">") e1) e2) n
  | n > eqPrec  = iConcat [iStr "(", eq, iStr ")"]
  | otherwise   = eq
  where
    eq = iConcat [pprExpr e1 eqPrec, iStr " > ", pprExpr e2 eqPrec]

pprExpr (EAp e1 e2) n
  | n == appPrec  = iConcat [iStr "(", app, iStr ")"]
  | otherwise     = app
  where
    app = iConcat [pprExpr e1 0, iSpace, pprExpr e2 appPrec]

pprExpr (ELet isrec defns expr) _ =
  iConcat [ iStr keyword, iNewline
          , iSpace, iIndent (pprDefns defns), iNewline
          ,  iStr "in ", pprExpr expr 0]
  where
    keyword
      | isrec     = "letrec"
      | otherwise = "let"

pprExpr (ECase e alts) _          =
  iConcat [ iStr "case ", pprExpr e 0, iStr " of", iNewline
          , iStr "  " , iIndent (iInterleave iSep (map pprAlt alts))]

pprExpr (ELam args body) _        =
  iConcat [ iStr "(\\", pprArgs args, iStr ". ", iIndent $ pprExpr body 0
          , iStr ")" ]

pprAlt :: CoreAlt -> Iseq
pprAlt (tag, args, rhs) = iConcat [  iStr "<", iNum tag, iStr "> "
                                  ,  pprArgs args,  iStr " -> "
                                  ,  iIndent $ pprExpr rhs 0]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave iSep (map pprDefn defns)

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, e) = iConcat [ iStr name, iStr " = "
                            , iIndent (pprExpr e 0) ]

pprArgs :: [Name] -> Iseq
pprArgs =  iInterleave iSpace.map iStr
