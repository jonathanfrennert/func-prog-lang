module PPrintTest where

import Syntax
import PPrint
import PPrintType
import Test.QuickCheck

-- | Measure the number of steps required to compute pretty printer.
-- Computational complexity should be linear (EX 1.4).

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s

prettyPrintLen :: Int -> Int
prettyPrintLen n = length.iDisplay.pprExpr $ mkMultiAp n (EVar "f") (EVar "x")

propTimeComplexity :: Int -> Property
propTimeComplexity n = (n > 0) ==> (res >= n - 10)
                             && (res <= 10 * n)
    where
      res = prettyPrintLen n

-- | Check if the layout works properly for ELet (EX 1.6) + (EX 1.7).

letLayoutExample :: CoreExpr
letLayoutExample = ELet True
  [("k", EVar "Hello, world"), ("n", EVar "Goodbye, world")]
  (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))

layoutTest :: IO ()
layoutTest = putStr.iDisplay.pprExpr $ letLayoutExample
