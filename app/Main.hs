module Main where

import Lang.PPrint
import Lang.StdPrelude

main :: IO ()
main = putStr.pprint $ preludeDefs
