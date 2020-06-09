module PrettyPrinter where

import Language

pprint :: CoreProgram -> String
pprint = undefined

pprExpr :: CoreExpr -> String
pprExpr (ENum n)    = show n
pprExpr (EVar v)    = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2

pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e  = pprExpr e
  | otherwise       = "(" ++ pprExpr e ++ ")"
