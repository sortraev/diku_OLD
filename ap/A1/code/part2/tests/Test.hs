import Test.Tasty
import Test.Tasty.HUnit


import OperateTests
import ApplyTests
import EvalTests
import AstTests

tests :: TestTree
tests = testGroup "All tests"
  [
   operateTests
  ,applyTests
  ,evalTests
  ,astTests
  ]

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests
