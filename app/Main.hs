module Main where

import Comp.Template

import System.IO
import Control.Monad

main :: IO ()
main = putStrLn.runProg $ "pair x y f = f x y ;\n"
                       ++ "fst p = p K ;\n"
                       ++ "snd p = p K1 ;\n"
                       ++ "f x y = letrec\n"
                       ++ "  a = pair x b ;\n"
                       ++ "  b = pair y a\n"
                       ++ " in\n fst (snd (snd (snd a))) ;\n"
                       ++ "main = f 3 4"

--main = forever $ do
--putStr "> "
--hFlush stdout
--line <- getLine
--putStrLn.runProg $ "main = " ++ line
