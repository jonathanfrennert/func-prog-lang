module TestUtils where

import Test.QuickCheck

runTestQC :: Testable prop => [prop] -> IO ()
runTestQC [] = return ()
runTestQC [p] = quickCheck p
runTestQC (p:ps) = do
  quickCheck p
  runTestQC ps
