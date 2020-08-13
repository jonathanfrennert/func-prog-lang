module TemplateTests (templateTests) where

import Comp.Show
import Comp.Eval
import Comp.Template
import Comp.TemplateBase
import Lang.Syntax
import Lang.Parser
import Lang.PPrintBase
import Utils.Heap
import Utils.Data
import Utils.Assoc

import Test.Hspec
import Control.Exception (evaluate)

templateTests :: IO ()
templateTests = hspec $ do
  describe "Template" $ do
    describe "compile" $ do
      it "can create an initial template state for a simple program." $ do
        (tiEqual (compile prog1) p1Init) `shouldBe` True

  describe "Eval" $ do
    describe "eval" $ do
      it "can evaluate a simple SKI combinator expression." $ do
        (res.last.eval.compile.parse $ skiProg) `shouldBe` (NNum 3)

    describe "scStep" $ do
      it "throws an error if a supercombinator or primitive has too few arguments." $ do
        evaluate (runProg fewArgProg) `shouldThrow` errorCall "Supercombinator S has too few arguments applied!"
      it "can evaluate recursive let statement." $ do
        (res.last.eval.compile.parse $ recProg) `shouldBe` (NNum 4)

    describe "instLet" $ do
      it "can produced the correct environment for let(rec)s." $ do
        letEx `shouldBe` envEx

  describe "Show" $ do
    describe "showStkTree" $ do
      it "can show a stack tree for an application node." $ do
        (iDisplay $ showStkTree treeHeap treeStack (iStr "") (iStr "") True) `shouldBe` stackTree

-- | Comparison functions

-- | Check if the given TiStates are equal. 'TiHeap' can only be approximately
-- compared as it holds an infinite list of unused addresses.
tiEqual :: TiState -> TiState -> Bool
tiEqual (stack, dump, heap, globals, stats) (stack', dump', heap', globals', stats')
  = stack == stack'
 && dump  == dump'
 && hEqual heap heap'
 && globals == globals'
 && stats == stats'

-- | Check if given heaps are equal. Unused addresses can only be approximately
-- compared as they are infinite lists.
hEqual :: Eq a => Heap a -> Heap a -> Bool
hEqual (Heap size free accs) (Heap size' free' accs') = size == size'
                                                    && equalInfs free free'
                                                    && accs == accs'

-- | Check if two infinite integer lists are approximately equal (first 10000 elements are equal).
equalInfs :: [Int] -> [Int] -> Bool
equalInfs xs ys = take 10000 xs == take 10000 ys

-- | See the contents a heap (for debugging only).
pprHeap :: Show a => Heap a -> String
pprHeap (Heap size _ accs) = concat ["Heap ", show size, " [..] ", show accs]


-- | 'compile' can handle a simple program (2.3.4).

prog1 :: CoreProgram
prog1 = [ ("main",[],ENum 3)
        , ("g",["x","y"], ELet False [("z",EVar "x")] (EVar "z"))
        , ("h",["x"], ECase (ELet False [("y",EVar "x")] (EVar "y")) [(1,[],ENum 2), (2,[],ENum 5)])]

p1Init :: TiState
p1Init = ( [1]
         , DummyTiDump
         , Heap (9,9,0,0) [10..]
             [ (9, NSupercomb "twice" ["f"] (EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) )
             , (8, NSupercomb "compose" ["f","g","x"] (EAp (EVar "f") (EAp (EVar "g") (EVar "x"))) )
             , (7, NSupercomb "S" ["f","g","x"] (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))) )
             , (6, NSupercomb "K1" ["x","y"] (EVar "y") )
             , (5, NSupercomb "K" ["x","y"] (EVar "x") )
             , (4, NSupercomb "I" ["x"] (EVar "x") )
             , (3, NSupercomb "h" ["x"] (ECase (ELet False [("y",EVar "x")] (EVar "y")) [(1,[],ENum 2), (2,[],ENum 5)]) )
             , (2, NSupercomb "g" ["x","y"] (ELet False [("z",EVar "x")] (EVar "z")) )
             , (1, NSupercomb "main" [] (ENum 3) )
             ]
         , [ ("main", 1)
           , ("g", 2)
           , ("h", 3)
           , ("I", 4)
           , ("K", 5)
           , ("K1", 6)
           , ("S", 7)
           , ("compose", 8)
           , ("twice", 9)
           ]
         , initialTiStat )


-- | The compiler can a run a simple program (EX 2.4).
skiProg :: String
skiProg = "main = S K K 3"

-- | The compiler will throw an error if a supercombinator or primitive has too few arguments (EX 2.5).
fewArgProg :: String
fewArgProg = "main = S K K"

-- | Show a single application stack tree.

treeStack :: TiStack
treeStack = [8]

treeHeap :: TiHeap
treeHeap = Heap (8,8,0,0) [9..]
    [ (8, NAp 7 6)
    , (7, NSupercomb "twice" ["f"] (EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) )
    , (6, NSupercomb "compose" ["f","g","x"] (EAp (EVar "f") (EAp (EVar "g") (EVar "x"))) )
    , (5, NSupercomb "S" ["f","g","x"] (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))) )
    , (4, NSupercomb "K1" ["x","y"] (EVar "y") )
    , (3, NSupercomb "K" ["x","y"] (EVar "x") )
    , (2, NSupercomb "I" ["x"] (EVar "x") )
    , (1, NSupercomb "main" [] (ENum 3) )
    ]

stackTree :: String
stackTree = "│   ┌── 6 (NSupercomb compose)\n└── 8\n    └── 7\n"

-- | The compiler can handle recursive let statements (EX 2.11).

recProg :: String
recProg = "pair x y f = f x y ;\n"
       ++ "fst p = p K ;\n"
       ++ "snd p = p K1 ;\n"
       ++ "f x y = letrec\n"
       ++ "  a = pair x b ;\n"
       ++ "  b = pair y a\n"
       ++ " in\n fst (snd (snd (snd a))) ;\n"
       ++ "main = f 3 4"

-- | The environment produced by a recursive let(rec) is as expected (the GHCI is a beautiful thing).

-- | Debug instantiating let(rec)s.
instLet' :: Bool -> [CoreDefn] -> CoreExpr -> TiHeap -> ASSOC Name Addr -> ASSOC Name Addr
instLet' isRec defs body heap env = new_env
  where
    (new_heap, addrs) = mapAccuml (\x y -> instantiate y x pass_env) heap (aRange defs)
    pass_env
      | isRec     = new_env
      | otherwise = env
    new_env           = def_bindings ++ env
    def_bindings      = zip (aDomain defs) addrs

letEx :: ASSOC Name Addr
letEx = instLet' True defs body heap env
  where
    defs = [ ("a", EAp (EAp (EVar "pair") (EVar "x")) (EVar "b"))
           , ("b", EAp (EAp (EVar "pair") (EVar "y")) (EVar "a"))]
    body = EAp (EVar "fst") (EAp (EVar "snd") (EAp (EVar "snd") (EAp (EVar "snd") (EVar "a"))))
    heap = Heap (15, 15,0,0) [16..] []
    env = []

envEx :: ASSOC Name Addr
envEx = [ ("a",17)
        , ("b",19) ]
