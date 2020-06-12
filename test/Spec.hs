import TestUtils
import PPrintTest

import Test.HUnit

main :: IO ()
main = do { runTestTT pprHTests
          ; runTestQC pprQTests
          }
