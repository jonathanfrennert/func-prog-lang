import Test.QuickCheck
import PrettyPrintTest

main :: IO ()
main = quickCheck propTimeComplexity
