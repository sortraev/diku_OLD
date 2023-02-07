module BoaTests.MiscTests where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaTests.Util

commentTests :: TestTree
commentTests = testGroup ">>>> Comment tests"
  [commentTest0, commentTest1, commentTest2, commentTest3, commentTest4] 

progTests :: TestTree
progTests = testGroup ">>>> Prog tests"
  [progTest0, progTest1, progTest2,
   progTest3, progTest4]

fooStmt0 = SExp (Var "x")
fooStmt1 = SDef "foo" (Const NoneVal)
fooProg  = [fooStmt0, fooStmt1]

progTest0 = test "simple statement program" "foo = None" [fooStmt1]
progTest1 = test "multiple statements" "x; foo = None" fooProg

progTest2 = negTest "empty program" ""
progTest3 = negTest "missing semicolon" "x foo = None" 
progTest4 = negTest "trailing semicolon (why is this invalid though)" "x; foo = None;" 

fooProg2 = [SDef "x" (Const (IntVal 5))]
commentTest0 = test "simple comment"                     "x = 5#hej der\n" fooProg2
commentTest1 = test "leading and trailing comments"      "#hej der\nx = 5#hej der\n" fooProg2
commentTest2 = test "multiple comments"                  "#hej der\n#hej der\n#hej der\nx = 5#hej der\n#hej der\n" fooProg2
commentTest3 = test "trailing comment, no newline after" "x = 5#hej der" fooProg2

commentTest4 = negTest "just a comment"                  "#just a comment\n" 
