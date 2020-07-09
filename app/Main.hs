module Main where

import Comp.Template

import System.IO
import Control.Monad

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  line <- getLine
  putStrLn.runProg $ "main = " ++ line
