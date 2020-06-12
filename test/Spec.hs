import Test.QuickCheck
import PPrintTest

main :: IO ()
main = do { quickCheck propTimeComplexity
          ; print layoutTest
          ; print newLineTest}
