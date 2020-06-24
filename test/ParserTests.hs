module ParserTests (parserTests) where

import Parser
import ParserBase
import Lexer
import Syntax

import Test.Hspec

parserTests :: IO ()
parserTests = hspec $ do
  describe "ParserBase" $ do
    describe "pThen" $ do
      it "can partially parse a BNF greeting." $ do
        (pGreeting $ clex greetingStr 0) `shouldBe` greetingP

    describe "pThen3" $ do
      it "can completely parse a BNF greeting." $ do
        (pGreeting3 $ clex greetingStr 0) `shouldBe` greetingP3

    describe "pZeroOrMore" $ do
      it "can parse zero or more tokens." $ do
        (pGreetingNo $ clex noGreetingStr 0) `shouldBe` noGreetingP
        (pGreetingNo $ clex greetingStr 0) `shouldBe` oneGreetingP

    describe "pOneOrMore" $ do
      it "can parse multiple tokens." $ do
        (pGreetingMult $ clex multGreetingStr 0) `shouldBe` multGreetingP

    describe "pApply" $ do
      it "can apply a function to the results of a parse." $ do
        (pGreetingN $ clex multGreetingStr 0) `shouldBe` nGreetingP

    describe "pOneOrMoreWithSep" $ do
      it "can parse one or more seperated greetings." $ do
        (pGreetingSep $ clex sepGreetingStr 0) `shouldBe` sepGreetingP

    describe "pNum" $ do
      it "can parse numbers." $ do
        (pNum $ clex numStr 0) `shouldBe` numP

  describe "Parser" $ do
    describe "parse" $ do
      it "can parse a simple Core language program" $ do
        (parse progStr) `shouldBe` coreProg

-- | Check if the 'pThen' can partially parse a greeting (basic BNF) (1.6.2)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
  where
    mk_pair hg name = (hg, name)

greetingStr :: String
greetingStr = "goodbye James!"

greetingP :: [ ( ( String, String ), [ Token ] ) ]
greetingP = [ ( ( "goodbye", "James" ), [(0, "!")] ) ]

-- | Check if 'pThen3' can parse a greeting (basic BNF) (1.6.3)

pGreeting3 :: Parser (String, String)
pGreeting3 = pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!")
    where
      mk_greeting hg name exclamation = (hg, name)

greetingP3 :: [ ( ( String, String ), [ Token ] ) ]
greetingP3 = [ ( ( "goodbye", "James" ), [] ) ]

-- | Check if 'pZeroOrMore' can parse a greeting BNF with no greeting (EX 1.13)

noGreetingStr :: String
noGreetingStr = " "

pGreetingNo :: Parser [(String, String)]
pGreetingNo = pZeroOrMore $ pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!")
    where
      mk_greeting hg name exclamation = (hg, name)

noGreetingP :: [ ( [ ( String, String ) ], [Token] ) ]
noGreetingP = [ ( [], [] ) ]

oneGreetingP :: [ ( [ ( String, String ) ], [Token] ) ]
oneGreetingP = [ ( [( "goodbye", "James" )], [] ) ]

-- | Check if 'pOneOrMore' can parse a greeting BNF with multiple greetings (EX 1.13)

multGreetingStr :: String
multGreetingStr = "goodbye James! hello Jerry!"

pGreetingMult :: Parser [(String, String)]
pGreetingMult = pOneOrMore $ pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!")
    where
      mk_greeting hg name exclamation = (hg, name)

multGreetingP :: [ ( [ ( String, String ) ], [Token] ) ]
multGreetingP = [ ([("goodbye","James"),("hello","Jerry")], []) ]

-- | Check if 'pApply' can count number of greetings in a greeting BNF with multiple greetings (EX 1.14)

pGreetingN :: Parser Int
pGreetingN = ( pOneOrMore $ pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!") ) `pApply` length
    where
      mk_greeting hg name exclamation = (hg, name)

nGreetingP :: [ ( Int, [Token] ) ]
nGreetingP = [ (2, []) ]

-- | Check if 'pOneOrMoreWithSep' can parse a multiple greetings seperated by semicolons (EX 1.15)

sepGreetingStr :: String
sepGreetingStr = "goodbye James!; hello Jerry!"

pGreetingSep :: Parser [(String, String)]
pGreetingSep = pOneOrMoreWithSep (pThen3 mk_greeting pHelloOrGoodbye pVar (pLit "!")) (pLit ";")
    where
      mk_greeting hg name exclamation = (hg, name)

sepGreetingP :: [ ( [ ( String, String ) ], [Token] ) ]
sepGreetingP = [ ([("goodbye","James"),("hello","Jerry")], []) ]

-- | Check if 'pNum' can parse a number (EX 1.18)

numStr :: String
numStr = " 11232323"

numP :: [ ( Int, [ Token ] ) ]
numP = [ (11232323, []) ]

-- | Check if parser can handle a simple program (EX 1.21)

progStr :: String
progStr = "f = 3 ;\ng x y = let z = x in z ;\nh x = case (let y = x in y) of\n  <1> -> 2 ;\n  <2> -> 5"

coreProg :: CoreProgram
coreProg = [("f",[],ENum 3),("g",["x","y"],ELet False [("z",EVar "x")] (EVar "z")),("h",["x"],ECase (ELet False [("y",EVar "x")] (EVar "y")) [(1,[],ENum 2),(2,[],ENum 5)])]
