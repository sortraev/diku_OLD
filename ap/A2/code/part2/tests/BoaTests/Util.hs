module BoaTests.Util where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaParser

test :: String -> String -> Program -> TestTree
test testName input expectedOut =
  testCase testName $ parseString input @?= (Right expectedOut)

negTest :: String -> String -> TestTree
negTest testName input = testCase testName $
  case parseString input of
    Left  _ -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p
