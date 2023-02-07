module BoaTests.ConstTests where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaTests.Util

constTests :: TestTree
constTests = testGroup ">>>> Const tests"
  [noneTests, trueTests, falseTests, intTests, strTests]


noneTests = testGroup ">> None const tests"
  [noneTest0, noneTest1, noneTest2, noneTest3, noneTest4, noneTest5, noneTest6]
trueTests = testGroup ">> True const tests"
  [trueTest0, trueTest1, trueTest2, trueTest3, trueTest4, trueTest5, trueTest6]
falseTests = testGroup ">> False const tests"
  [falseTest0, falseTest1, falseTest2, falseTest3, falseTest4, falseTest5, falseTest6]
intTests = testGroup ">> Int const tests" 
  [intTest0, intTest1, intTest2, intTest3, intTest4, intTest5, intTest6]
strTests = testGroup ">> String literal tests"
  [strTest0, strTest1, strTest2, strTest3, strTest4, strTest5, strTest6, strTest7, strTest8]


noneTest0 = test "None"                     "None"  [SExp (Const NoneVal)]
noneTest1 = test "Trailing garbage"         "Nonex" [SExp (Var "Nonex")]
noneTest2 = test "Leading garbage"          "xNone" [SExp (Var "xNone")]
noneTest3 = test "Leading whitespace"       " None" [SExp (Const NoneVal)]
noneTest4 = test "Trailing whitespace"      "None " [SExp (Const NoneVal)]
noneTest5 = test "Missing capitalization"   "none"  [SExp (Var "none")]
noneTest6 = negTest "Whitespace in keyword" "No ne"


trueTest0 = test "True"                     "True"    [SExp (Const TrueVal)]
trueTest1 = test "Trailing garbage"         "Truex"   [SExp (Var "Truex")]
trueTest2 = test "Leading garbage"          "xTrue"   [SExp (Var "xTrue")]
trueTest3 = test "Leading whitespace"       " True"   [SExp (Const TrueVal)]
trueTest4 = test "Trailing whitespace"      "True "   [SExp (Const TrueVal)]
trueTest5 = test "Missing capitalization"   "true"    [SExp (Var "true")]
trueTest6 = negTest "Whitespace in keyword" "Tr ue"


falseTest0 = test "False"                    "False"  [SExp (Const FalseVal)]
falseTest1 = test "Trailing garbage"         "Falsex" [SExp (Var "Falsex")]
falseTest2 = test "Leading garbage"          "xFalse" [SExp (Var "xFalse")]
falseTest3 = test "Leading whitespace"       " False" [SExp (Const FalseVal)]
falseTest4 = test "Trailing whitespace"      "False " [SExp (Const FalseVal)]
falseTest5 = test "Missing capitalization"   "false"  [SExp (Var "false")]
falseTest6 = negTest "Whitespace in keyword" "Fal se"


intTest0 = test "Just a number"       "43"   [SExp (Const (IntVal 43))]
intTest1 = test "Leading whitespace"  " 43"  [SExp (Const (IntVal 43))]
intTest2 = test "Trailing whitespace" "43 "  [SExp (Const (IntVal 43))]
intTest3 = test "Whitespace"          " 43 " [SExp (Const (IntVal 43))]
intTest4 = test "Neg number"          "-43"  [SExp (Const (IntVal (-43)))]
intTest5 = test "Legal space before negation" " -43"  [SExp (Const (IntVal (-43)))]
intTest6 = negTest "Illegal space after negation" "- 43"


strTest0 = test "simple string" "'foo'" [SExp (Const (StringVal "foo"))]
strTest1 = test "empty string"  "''"   [SExp (Const (StringVal ""))]
strTest2 = test "comment in string" "'fooabc#commentinstring'" 
             [SExp (Const (StringVal "fooabc#commentinstring"))]
strTest3 = test "Test non-vanilla Boa chars" "'\\\n'" [SExp (Const (StringVal "\\\n"))]
strTest4 = test "Non-vanilla Boa single-quote " "'foo''" [SExp (Const (StringVal "foo'"))]
strTest5 = negTest "Uon-printable characters" "'\DEL'"
strTest6 = test "Whitespace before" "\t   'foo'" [SExp (Const (StringVal "foo"))]
strTest7 = test "Whitespace after" "'foo'  \n  " [SExp (Const (StringVal "foo"))]
strTest8 = negTest "More non-printable characters" "'\aL\CR'"








