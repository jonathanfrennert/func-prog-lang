module PrettyPrintOps where

import Syntax
import PrettyPrintBase

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave (iAppend (iStr " ;") iNewline) (map pprSc prog)

pprSc :: CoreScDefn -> Iseq
pprSc (name, args, body) = iConcat [ iStr name, iSpace, pprArgs args
                                   , iStr " = ", iIndent $ pprExpr body]

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n)                = iNum n
pprExpr (EVar v)                = iStr v
pprExpr (EAp e1 e2)             = iConcat [pprExpr e1, iSpace, pprAExpr e2]
pprExpr (ELet isrec defns expr) =
  iConcat [ iStr keyword, iNewline
          , iSpace, iIndent (pprDefns defns), iNewline
          ,  iStr "in ", pprExpr expr]
  where
    keyword
      | isrec     = "letrec"
      | otherwise = "let"
pprExpr (ECase e alts)       =
  iConcat [ iStr "case ", pprExpr e, iStr " of", iNewline
          , iStr "  " , iIndent (iInterleave iSep (map pprAlt alts))]
pprExpr (ELam args body)
  = iConcat [ iStr "(\\", pprArgs args, iStr ". ", iIndent $ pprExpr body
            , iStr ")" ]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e  = pprExpr e
  | otherwise       = iConcat [iStr "(", pprExpr e, iStr ")"]

pprAlt :: CoreAlt -> Iseq
pprAlt (tag, args, rhs) = iConcat [  iStr "<", iNum tag, iStr "> "
                                  ,  pprArgs args,  iStr " -> "
                                  ,  iIndent $ pprExpr rhs ]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave iSep (map pprDefn defns)

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, e) = iConcat [ iStr name, iStr " = "
                            , iIndent (pprExpr e) ]

pprArgs :: [Name] -> Iseq
pprArgs =  iInterleave iSpace.map iStr
