module PrettyPrinterTest (propPrettyPrint) where

import Language
import PrettyPrinter
import Test.QuickCheck

-- | Measure the number of steps required to compute pretty printer.
-- Computational complexity should be O(n^2) (EX 1.1).

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s

prettyPrintLen :: Int -> Int
prettyPrintLen n = length $ pprExpr $ mkMultiAp n (EVar "f") (EVar "x")

propPrettyPrint :: Int -> Property
propPrettyPrint n = (n > 0) ==> (res >= n2 - 100000)
                             && (res <= 1000000 * n2)
    where
      n2 = n * n
      res = prettyPrintLen n
