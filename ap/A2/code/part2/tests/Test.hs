import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaParser

import BoaTests.ConstTests
import BoaTests.ExpTests
import BoaTests.MiscTests

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) allTests

allTests :: TestTree
allTests = testGroup "My BoaParser unit tests"
  [
  constTests, expTests, progTests, commentTests
  ]
