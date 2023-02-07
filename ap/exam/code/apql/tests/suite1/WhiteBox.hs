import Test.Tasty
import Test.Tasty.HUnit

import ParserTests
import PreprocessorTests

main :: IO()
main = defaultMain $ localOption (mkTimeout 1000000) allTests

allTests :: TestTree
allTests = testGroup "My APQL unit tests"
  [
   parserTests
  ,preprocessorTests
  ]
