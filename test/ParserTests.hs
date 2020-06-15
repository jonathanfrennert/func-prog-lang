module ParserTests (parserTests) where

import Parser
import Syntax

import Test.Hspec
import Test.QuickCheck

parserTests :: IO ()
parserTests = hspec $ do
  describe "clex" $ do
    it "can identify digits, variables and exclude whitespaces." $ do
      (clex ex1Str 0) `shouldBe` ex1

    it "can ignore comments." $ do
      (clex comStr 0) `shouldBe` comToks

    it "can identify two-char operations." $ do
      (clex eqStr 0) `shouldBe` eqToks

-- | Check if lexer can digits, variables and spaces (1.6.1)

ex1Str :: String
ex1Str = "123ab  c_de"

ex1 :: [Token]
ex1 = [ (0, "123"), (0, "ab"), (0, "c_de") ]

-- | Check if the lexer can ignore comments and handle multiple lines (EX 1.9, 1.11).

comStr :: String
comStr = "123ab  -- HELLLOOOOOOO\nc_de"

comToks :: [Token]
comToks = [ (0 , "123"), (0, "ab"), (1, "c_de") ]

-- | Check if the lexer can identify 'twoCharOps' (EX 1.10)

eqStr :: String
eqStr = "123ab  ==c_de"

eqToks :: [Token]
eqToks = [ (0 , "123"), (0, "ab"), (0, "=="), (0, "c_de") ]
