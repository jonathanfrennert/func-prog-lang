module LexerTests (lexerTests) where

import Lang.Lexer

import Test.Hspec

lexerTests :: IO ()
lexerTests = hspec $ do
  describe "Lexer" $ do
    describe "clex" $ do
      it "can identify digits, variables and exclude whitespaces." $ do
        (clex digitSpaceVarStr 0) `shouldBe` res1

      it "can ignore comments." $ do
        (clex commentStr 0) `shouldBe` res2

      it "can identify two-char operations." $ do
        (clex binOpStr 0) `shouldBe` res3

-- | Check if lexer can digits, variables and spaces (1.6.1)

digitSpaceVarStr :: String
digitSpaceVarStr = "123ab  c_de"

res1 :: [Token]
res1 = [ (0, "123"), (0, "ab"), (0, "c_de") ]

-- | Check if the lexer can ignore comments and handle multiple lines (EX 1.9, 1.11).

commentStr :: String
commentStr = "123ab  -- HELLLOOOOOOO\nc_de"

res2 :: [Token]
res2 = [ (0 , "123"), (0, "ab"), (1, "c_de") ]

-- | Check if the lexer can identify 'twoCharOps' (EX 1.10)

binOpStr :: String
binOpStr = "123ab  ==c_de"

res3 :: [Token]
res3 = [ (0 , "123"), (0, "ab"), (0, "=="), (0, "c_de") ]
