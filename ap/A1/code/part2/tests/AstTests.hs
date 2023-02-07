module AstTests where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaInterp

------------------------------
----- programs from ASTs -----
------------------------------
astTests :: TestTree
astTests = testGroup "Programs in ASTs from files"
  [
   testProgFromFile "misc" Nothing
  ,testProgFromFile "contrived_comprehension" Nothing
  ,testProgFromFile "variable_overshadowing" Nothing
  ,testProgFromFile "print_bomb" Nothing
  ,testProgFromFile "name_error"     (Just (EBadVar "composites"))
  ,testProgFromFile "zero_div_error" (Just (EBadArg "Div error: zero divisor"))
  -- TODO: add more negative tests
  ]


baseTestDir = "tests/progs/"

testProgFromFile :: String -> Maybe RunError -> TestTree
testProgFromFile testName maybeError =
  let ast_file  = baseTestDir ++ testName ++ ".ast"
      out_file  = baseTestDir ++ testName ++ ".out"
      test_name = "Running AST from " ++ ast_file

  in testCase test_name $
       read <$> readFile ast_file
         >>= \ast_in -> readFile out_file
         >>= \expected_out -> execute ast_in @?= (lines expected_out, maybeError)
