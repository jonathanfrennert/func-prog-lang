module PPrintTest where

import Syntax
import PPrint
import PPrintType
import TestUtils

import qualified Test.HUnit as H
import qualified Test.QuickCheck as Q

pprHTests :: H.Test
pprHTests = H.TestList
  [ H.TestLabel "pprLayout" layoutTest
  , H.TestLabel "pprNewline" newLineTest
  ]

pprQTests = [propTimeComplexity]

pprCore :: CoreExpr -> String
pprCore = iDisplay.pprExpr

-- | Measure the number of steps required to compute pretty printer.
-- Computational complexity should be linear (EX 1.4).

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s

prettyPrintLen :: Int -> Int
prettyPrintLen n = length.iDisplay.pprExpr $ mkMultiAp n (EVar "f") (EVar "x")

propTimeComplexity :: Int -> Q.Property
propTimeComplexity n = (n > 0) Q.==> (res >= n - 10)
                             && (res <= 10 * n)
    where
      res = prettyPrintLen n

-- | Check if the layout works properly for 'ELet' (EX 1.6).

letLayoutExample :: CoreExpr
letLayoutExample = ELet True
  [("k", EVar "Hello, world"), ("n", EVar "Goodbye, world")]
  (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))

letLayoutStr :: String
letLayoutStr = "letrec\n k = Hello, world;\n n = Goodbye, world\nin f x (g x)"

layoutTest :: H.Test
layoutTest = H.TestCase (
  H.assertEqual "Pretty print ofletrec statements is incorrect"
  (pprCore letLayoutExample)
  letLayoutStr)

-- | Check if layout works when printing string with newline character (EX 1.7)

newLineExpr :: CoreExpr
newLineExpr = EVar "newline\nnewline\n,give me a two time"

newLineStr :: String
newLineStr = "newline\nnewline\n,give me a two time"

newLineTest :: H.Test
newLineTest = H.TestCase (
  H.assertEqual "Pretty print of EVar with newline is incorrect"
  (pprCore newLineExpr)
  newLineStr)
