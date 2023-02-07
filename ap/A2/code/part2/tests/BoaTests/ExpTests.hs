module BoaTests.ExpTests where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaTests.Util

expTests :: TestTree
expTests = testGroup ">>>> Exp tests"
  [identAndVarTests
  ,operTests
  ,notTests
  ,callTests
  ,listTests
  ,comprTests]


notTests = testGroup ">> Not expression tests"
  [notTest0, notTest1, notTest2, notTest3, notTest4,
   notTest5, notTest6, notTest7, notTest8, notTest9]

identAndVarTests = testGroup ">> Ident, and Var expression tests"
  [identTest0, identTest1, identTest2, identTest3, identTest4, identTest5,
   identTest6, identTest7, identTest8, identTest9, identTest10, identTest11,
   identTest12, identTest13, identTest14, identTest15]

operTests = testGroup ">> Oper expression tests"
  [relationalBinopTests, arithmeticBinopTests]
arithmeticBinopTests = testGroup ">> Arithmetic binop expression tests"
  [plusTest0, minusTest0, timesTest0, divTest0, modTest0,
   plusTest1, minusTest1, timesTest1, divTest1, modTest1]
relationalBinopTests = testGroup ">> Relational binop expression tests"
  [eqTest0, lessTest0, greaterTest0, inTest0,
   neqTest0, lessEqTest0, greaterEqTest0, notInTest0,
   eqTest1, lessTest1, greaterTest1, inTest1,
   neqTest1, lessEqTest1, greaterEqTest1, notInTest1]

callTests = testGroup ">> Call expression tests"
  [callTest0, callTest1, callTest2, callTest3, callTest4,
   callTest5, callTest6, callTest7, callTest8, callTest9]

listTests = testGroup ">> List expression tests"
  [listTest0, listTest1, listTest2, listTest3, listTest4, listTest5, listTest6]

comprTests = testGroup ">> Comprehension expression tests"
  [comprTest0, comprTest1, comprTest2, comprTest3, comprTest4, comprTest5,
   comprTest6, comprTest7, comprTest8, comprTest9, comprTest10]


identTest0 = test "simple ident" "foo" [SExp (Var "foo")]
identTest1 = test "leading whitespace" " \n foo" [SExp (Var "foo")]
identTest2 = test "trailing whitespace" "foo \t\t\t" [SExp (Var "foo")]
identTest3 = test "leading underscore" " _foo" [SExp (Var "_foo")]
identTest4 = test "non-leading numerics" " _f123123oo" [SExp (Var "_f123123oo")]
identTest5 = test "almost a reserved keyword" "foR = 3" [SDef "foR" (Const (IntVal 3))]
identTest6 = test "test correct whitespace after ident" "foo in [foo]" [SExp (Oper In (Var "foo") (List [Var "foo"]))]
identTest7 = test "legal leading/trailing characters 1" "x+y" [SExp (Oper Plus (Var "x") (Var "y"))]
identTest8 = test "legal leading/trailing characters 2" "x+(y)" [SExp (Oper Plus (Var "x") (Var "y"))]
identTest9 = test "legal leading/trailing characters 3" "x(_y)" [SExp (Call "x" [Var "_y"])]
identTest10 = negTest "leading numerics" "42foo = -42"
identTest11 = negTest "reserved keyword" "for = 3"
identTest12 = negTest "illegal characters 1" "!foo = 3"
identTest13 = negTest "illegal characters 2" "f$oo = 3"
identTest14 = negTest "trailing keyword" "fooin [foo]"
identTest15 = negTest "leading keyword" "[foo] inbar"


a = Const (IntVal 1337)
b = Const (IntVal 42)
c = Const (IntVal 3)
plusTest0 = test "simple add expression" "1337+42"  [SExp (Oper Plus a b)]
plusTest1 = test "correct association of add" "1337+42+3" [SExp (Oper Plus (Oper Plus a b) c)]
minusTest0 = test "simple minus expression" "1337-42" [SExp (Oper Minus a b)]
minusTest1 = test "correct association of sub" "1337-42-3" [SExp (Oper Minus (Oper Minus a b) c)]

timesTest0 = test "simple times expression" "1337*42"  [SExp (Oper Times a b)]
timesTest1 = test "correct association of times" "1337*42*3" [SExp (Oper Times (Oper Times a b) c)]

divTest0 = test "simple div expression" "1337//42"  [SExp (Oper Div a b)]
divTest1 = test "correct association of div" "1337//42//3" [SExp (Oper Div (Oper Div a b) c)]

modTest0 = test "simple mod expression" "1337%42"  [SExp (Oper Mod a b)]
modTest1 = test "correct association of mod" "1337%42%3" [SExp (Oper Mod (Oper Mod a b) c)]

eqTest0      = test "simple == expression" "1337 == 42" [SExp (Oper Eq a b)]
lessTest0    = test "simple < expression" "1337 < 42" [SExp (Oper Less a b)]
greaterTest0 = test "simple > expression" "1337 > 42" [SExp (Oper Greater a b)]
inTest0      = test "simple `in` expression" "1337 in [42]" [SExp (Oper In a (List [b]))]

neqTest0      = test "simple != expression" "1337 != 42" [SExp (Not (Oper Eq a b))]
lessEqTest0    = test "simple <= expression" "1337 <= 42" [SExp (Not (Oper Greater a b))]
greaterEqTest0 = test "simple >= expression" "1337 >= 42" [SExp (Not (Oper Less a b))]
notInTest0      = test "simple `not in` expression" "1337 not in [42]" [SExp (Not (Oper In a (List [b])))]

eqTest1      = negTest "correct non-assocation of ==" "1337 == 42 == 3"
lessTest1    = negTest "correct non-assocation of <" "1337 < 42 < 3"
greaterTest1 = negTest "correct non-assocation of >" "1337 > 42 > 3"
inTest1      = negTest "correct non-assocation of `in`" "3 in [3] in [True]" -- thank god!

neqTest1       = negTest "correct non-assocation of !=" "1337 != 42 != 3"
lessEqTest1    = negTest "correct non-assocation of <=" "1337 <= 42 <= 3"
greaterEqTest1 = negTest "correct non-assocation of >=" "1337 >= 42 >= 3"
notInTest1     = negTest "correct non-assocation of `not in`" "3 not in [3] not in [True]" -- thank god!


operPrecTest0 = test "inter-arithOp precedence 1" "1337 + 3 * 42"
  [SExp (Oper Plus (Const (IntVal 1337)) (Oper Times (Const (IntVal 3)) (Const (IntVal 42))))]

operPrecTest1 = test "inter-arithOp precedence 2" "1337 + 3 // 42"
  [SExp (Oper Plus (Const (IntVal 1337)) (Oper Div (Const (IntVal 3)) (Const (IntVal 42))))]

operPrecTest2 = test "inter-arithOp precedence 3" "1337 + 3 % 42"
  [SExp (Oper Plus (Const (IntVal 1337)) (Oper Mod (Const (IntVal 3)) (Const (IntVal 42))))]

operPrecTest3 = test "inter-arithOp precedence 4" "1337 - 3 * 42"
  [SExp (Oper Minus (Const (IntVal 1337)) (Oper Times (Const (IntVal 3)) (Const (IntVal 42))))]

operPrecTest4 = test "inter-arithOp precedence 5" "1337 - 3 // 42"
  [SExp (Oper Minus (Const (IntVal 1337)) (Oper Div (Const (IntVal 3)) (Const (IntVal 42))))]

operPrecTest5 = test "inter-arithOp precedence 6" "1337 - 3 % 42"
  [SExp (Oper Minus (Const (IntVal 1337)) (Oper Mod (Const (IntVal 3)) (Const (IntVal 42))))]

operPrecTest6 = test "relOp/arithOp precedence 1" "143 + 5 < 2 % 2"
  [SExp (Oper Less (Oper Plus (Const (IntVal 143)) (Const (IntVal 5))) (Oper Mod (Const (IntVal 2)) (Const (IntVal 2))))]

operPrecTest7 = test "relOp/arithOp precedence 2" "143 + 5 < 2 % 2"
  [SExp (Oper Less (Oper Plus (Const (IntVal 143)) (Const (IntVal 5))) (Oper Mod (Const (IntVal 2)) (Const (IntVal 2))))]

operPrecTests = testGroup "Inter-Oper precedence tests"
  [operPrecTest0, operPrecTest1, operPrecTest2, operPrecTest3,
   operPrecTest4, operPrecTest5, operPrecTest6, operPrecTest7]


notTest0 = test "simple not exp" "not 0" [SExp (Not (Const (IntVal 0)))]
notTest1 = test "nested not exp" "not not not 0" [SExp (Not (Not (Not (Const (IntVal 0)))))]
notTest2 = test "correct precedence 1" "not 3 + 4" [SExp (Not (Oper Plus (Const (IntVal 3)) (Const (IntVal 4))))]
notTest3 = test "correct precedence 2" "not 3 * 4" [SExp (Not (Oper Times (Const (IntVal 3)) (Const (IntVal 4))))]
notTest4 = test "correct precedence 3" "not 3 < 4" [SExp (Not (Oper Less (Const (IntVal 3)) (Const (IntVal 4))))]
notTest5 = test "correct precedence 4" "not foo(x)" [SExp (Not (Call "foo" [Var "x"]))]
notTest6 = test "not with inequality"  "not 3 != 4" [SExp (Not (Not (Oper Eq (Const (IntVal 3)) (Const (IntVal 4)))))]
notTest7 = test "not with list exp"    "not [3, 4, 5] != 4" [SExp (Not (Not (Oper Eq (List
             [Const (IntVal 3),Const (IntVal 4),Const (IntVal 5)]) (Const (IntVal 4)))))]
notTest8 = negTest "illegal association 1" "not 3 + not 4"
notTest9 = negTest "illegal association 2" "3 % not 4"


fooCallExp = [SExp (Call "main" [Const (IntVal 1), Const (StringVal "hej"), Const NoneVal])]
callTest0 = test "simple function call" "main(1, 'hej', None)" fooCallExp
callTest1 = test "one argument" "main(123)" [SExp (Call "main" [Const (IntVal 123)])]
callTest2 = test "no arguments"         "main()" [SExp (Call "main" [])]
callTest3 = test "whitespace in args" "main( 1 , 'hej' , None )" fooCallExp
callTest4 = test "whitespace everywhere" "main ( 1 , 'hej' , None ) " fooCallExp
callTest5 = test "no whitespace" "main(1,'hej',None)" fooCallExp
callTest6 = negTest "missing arg delimeter" "main(1 'hej', None)"
callTest7 = negTest "missing arg after delimeter" "main(1, 'hej', None,)"
callTest8 = negTest "missing parenthesis 1" "main(1, 'hej', None"
callTest9 = negTest "missing parenthesis 2" "main1, 'hej', None)"


fooListProg = [SExp (List [Const (IntVal 1), Const (IntVal 2), Const (IntVal 3)])]
emptyListProg = [SExp (List [])]
listTest0 = test "simple list" "[1, 2, 3]" fooListProg
listTest1 = test "whitespace all over" " [ 1 , 2 , 3 ] " fooListProg
listTest2 = test "no whitespace" "[1,2,3]" fooListProg
listTest3 = test "empty list" "[]" emptyListProg
listTest4 = negTest "missing delimeter" "[1, 2, 3 4]"
listTest5 = negTest "missing closing bracket" "[1, 2, 3, 4"
listTest6 = negTest "missing opening bracket" "1, 2, 3, 4]"


fooComprExp = [SExp (Compr (List [Const (IntVal 1),Const (IntVal 2),
               Const (IntVal 3)]) [CCFor "x" (List [Var "a",Var "b",Var "c"])])]
comprTest0 = test "simple comprehension expression" "[[1, 2, 3] for x in [a, b, c]]" fooComprExp
comprTest1 = test "extra whitespace" " [ [1, 2, 3] for x in [a, b, c] ] " fooComprExp
comprTest2 = test "minimal whitespace" "[[1, 2, 3]for x in[a, b, c]]" fooComprExp
comprTest3 = test "interleaved if and for clauses" "[x for y in z if 3 < x for z in a if None]"
               [SExp (Compr (Var "x") [CCFor "y" (Var "z"),CCIf (Oper Less (Const (IntVal 3))
                  (Var "x")),CCFor "z" (Var "a"),CCIf (Const NoneVal)])]
comprTest4 = test "`in` relOp not confused with `for` clause" "[x in y for a in c]"
               [SExp (Compr (Oper In (Var "x") (Var "y")) [CCFor "a" (Var "c")])]
comprTest5 = test "list in comprehension" "[1, 4, None, [x for y in z], True]"
               [SExp (List [Const (IntVal 1),Const (IntVal 4),Const NoneVal,Compr (Var "x")
               [CCFor "y" (Var "z")],Const TrueVal])]
comprTest6 = test "comprehension in comprehension" "[[y] for y in [z+3 for z in [yes]]]"
               [SExp (Compr (List [Var "y"]) [CCFor "y" (Compr (Oper Plus (Var "z")
               (Const (IntVal 3))) [CCFor "z" (List [Var "yes"])])])]
comprTest7  = negTest "missing whitespace 1" "[[1, 2, 3]fory in[a, b, c]]"
comprTest8  = negTest "missing whitespace 2" "[[1, 2, 3]for yin[a, b, c]]"
comprTest9 = negTest "missing whitespace 3" "[xfor yin]"
comprTest10 = negTest "no leading for clause" "[if True]"
