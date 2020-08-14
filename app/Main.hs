module Main where

import Comp.Template

import System.IO
import Control.Monad

main :: IO ()
main = putStrLn.runProg $ "id x = x ;\n"
       ++ "main = twice twice id 3"

--main = forever $ do
--  putStr "> "
--  hFlush stdout
--  line <- getLine
--  putStrLn.runProg $ "main = " ++ line
