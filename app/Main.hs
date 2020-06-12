module Main where

import PPrint
import StdPrelude

main :: IO ()
main = putStr.pprint $ preludeDefs 
