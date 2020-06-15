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

    it "can ignore comments." $ do
      (clex comStr) `shouldBe` comToks

    it "can identify two-char operations." $ do
      (clex eqStr) `shouldBe` eqToks

-- | Check if lexer can digits, variables and spaces (1.6.1)

ex1Str :: String
ex1Str = "123ab  c_de"

ex1 :: [Token]
ex1 = ["123", "ab", "c_de"]

-- | Check if the lexer can ignore comments (EX 1.9)

comStr :: String
comStr = "123ab  -- HELLLOOOOOOO\nc_de"

comToks :: [Token]
comToks = ["123", "ab", "c_de"]

-- | Check if the lexer can identify 'twoCharOps' (EX 1.10)

eqStr :: String
eqStr = "123ab  ==c_de"

eqToks :: [Token]
eqToks = ["123", "ab", "==", "c_de"]
