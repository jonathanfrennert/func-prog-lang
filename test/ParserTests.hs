module ParserTests (parserTests) where

import Parser
import Syntax

import Test.Hspec
import Test.QuickCheck

parserTests :: IO ()
parserTests = hspec $ do
  describe "clex" $ do
    it "can identify digits, variables and exclude whitespaces." $ do
      (clex ex1Str) `shouldBe` ex1

-- | Check if lexer can handle digits, variables and spaces (1.6.1).

ex1Str :: String
ex1Str = "123ab  c_de"

ex1 :: [Token]
ex1 = ["123", "ab", "c_de"]
