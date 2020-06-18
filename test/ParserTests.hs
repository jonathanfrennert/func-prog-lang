module ParserTests (parserTests) where

import Lexer
import Parser

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

    it "can parse simple greeting BNF." $ do
      (pGreeting $ clex greetingStr 0) `shouldBe` greetingP

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

-- | Check if the parsing can handle greetings (basic BNF) (1.6.2).

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
  where
    mk_pair hg name = (hg, name)

greetingStr :: String
greetingStr = "goodbye James!"

greetingP :: [((String, String), [Token])]
greetingP = [ ( ( "goodbye", "James" ), [(0, "!")] ) ]
