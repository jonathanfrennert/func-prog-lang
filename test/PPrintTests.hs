module PPrintTests(pprTests) where

import Syntax
import PPrint
import PPrintType

import Test.Hspec
import Test.HUnit
import Test.QuickCheck

pprTests :: IO ()
pprTests = hspec $ do
  describe "pprExpr" $ do
    it "has a linear time complexity." $ property $
      propTimeComplexity

    it "can print letrec." $ do
      (pprCore letLayoutExample) `shouldBe` letLayoutStr

    it "can print EVar with newline." $ do
      (pprCore newLineExpr) `shouldBe` newLineStr

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

propTimeComplexity :: Int -> Property
propTimeComplexity n = (n > 0) ==> (res >= n - 10)
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

-- | Check if layout works when printing string with newline character (EX 1.7)

newLineExpr :: CoreExpr
newLineExpr = EVar "newline\nnewline\n,give me a two time"

newLineStr :: String
newLineStr = "newline\nnewline\n,give me a two time"
