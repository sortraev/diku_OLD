module OperateTests where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaInterp
-------------------------
----- operate tests -----
-------------------------
a' = 42
b' = -1337

a = IntVal a'
b = IntVal b'
zero = IntVal 0
one = IntVal 1

plus1 = operate Plus a b
plus2 = operate Plus a zero
plus3 = operate Plus zero a

minus1 = operate Minus a b
minus2 = operate Minus a zero
minus3 = operate Minus zero a

times1 = operate Times a b
times2 = operate Times a zero
times3 = operate Times zero b
times4 = operate Times b one
times5 = operate Times one a

div1 = operate Div a b
div2 = operate Div a zero
div3 = operate Div zero b
div4 = operate Div b one
div5 = operate Div one a
div6 = operate Div one b
div7 = operate Div one one

mod1 = operate Mod a b
mod2 = operate Mod a zero
mod3 = operate Mod zero b
mod4 = operate Mod b one
mod5 = operate Mod one b

less1 = operate Less a b
less2 = operate Less b a
less3 = operate Less a a
greater1 = operate Greater a b
greater2 = operate Greater b a
greater3 = operate Greater a a

eq1 = operate Eq a a
eq2 = operate Eq a b
eq3 = operate Eq TrueVal FalseVal
eq4 = operate Eq NoneVal (ListVal [IntVal 3, StringVal "hej"])
eq5 = operate Eq (StringVal "abba") (StringVal "abbc")
eq6 = operate Eq (StringVal "123") (StringVal "123")


fooList1 = [TrueVal, IntVal 3, StringVal "hey", NoneVal, StringVal "hej", IntVal 4]
fooList2 = [TrueVal, IntVal 3, StringVal "hey!", NoneVal, StringVal "hej", IntVal 4]
fooList3 = (replicate 10000 (ListVal fooList1)) ++ [ListVal fooList2] ++
           (replicate 10000 (ListVal fooList1))

in1 = operate In (IntVal 3) (ListVal [])
in2 = operate In (StringVal "hej") (ListVal fooList1)
in3 = operate In TrueVal (ListVal [FalseVal, IntVal 3])
in4 = operate In (IntVal 1728329) (ListVal $ map IntVal [1..1728329*2])
in5 = operate In TrueVal (ListVal $ map IntVal [1..1728329*2])
in6 = operate In (ListVal fooList2) (ListVal fooList3)
in7 = operate In (ListVal fooList3) (ListVal fooList3)


operateTests :: TestTree
operateTests = testGroup ("operate tests - with (a, b) := (" ++
                          show a' ++ ", " ++ show b' ++ ")")
  [testCase "Plus1: a + b"       $ plus1  @?= (Right $ IntVal $ a' + b')
  ,testCase "Plus2: a + 0"       $ plus2  @?= (Right a)
  ,testCase "Plus3: 0 + a"       $ plus3  @?= (Right a)

  ,testCase "Minus1: a - 0"      $ minus2  @?= (Right a)
  ,testCase "Minus2: 0 - a"      $ minus3  @?= (Right $ IntVal (-a'))

  ,testCase "Minus3: a - b"      $ minus1  @?= (Right $ IntVal $ a' - b')
  ,testCase "Minus4: a - 0"      $ minus2  @?= (Right a)
  ,testCase "Minus5: 0 - a"      $ minus3  @?= (Right $ IntVal (-a'))

  ,testCase "Times1: a * b"      $ times1  @?= (Right $ IntVal $ a' * b')
  ,testCase "Times2: a * 0 == 0" $ times2  @?= (Right zero)
  ,testCase "Times3: 0 * b == 0" $ times3  @?= (Right zero)
  ,testCase "Times4: b * 1 == b" $ times4  @?= (Right b)
  ,testCase "Times5: 1 * a == a" $ times5  @?= (Right a)

  ,testCase "Div1: a / b"                  $ div1  @?= (Right $ IntVal $ div a' b')
  ,testCase "Div2: zero div error"         $ div2  @?= (Left "Div error: zero divisor")
  ,testCase "Div3: 0 / b == 0"             $ div3  @?= (Right zero)
  ,testCase "Div4: b / 1 == b"             $ div4  @?= (Right b)
  ,testCase "Div5: 1 / a == 0 for a > 1"   $ div5  @?= (Right zero)
  ,testCase "Div6: 1 / b == -1 for b < 0"  $ div6  @?= (Right $ IntVal (-1))
  ,testCase "Div7: 1 / 1 == 1"             $ div7  @?= (Right one)

  ,testCase "Mod1: a % b"                  $ mod1  @?= (Right $ IntVal $ mod a' b')
  ,testCase "Mod2: zero div error"         $ mod2  @?= (Left "Mod error: zero divisor")
  ,testCase "Mod3: 0 % a == 0"             $ mod3  @?= (Right zero)
  ,testCase "Mod4: 1 % 1 == 0 for b < 0"   $ mod4  @?= (Right zero)
  ,testCase "Mod5: 1 % b == b+1 for b < 0" $ mod5  @?= (Right $ IntVal $ b' + 1)

  ,testCase "Less1: a < b"              $ less1 @?= (Right $ FalseVal)
  ,testCase "Less2: b < a"              $ less2 @?= (Right $ TrueVal)
  ,testCase "Less3: a < a"              $ less3 @?= (Right $ FalseVal)

  ,testCase "Greater1: a < b"           $ greater1 @?= (Right $ TrueVal)
  ,testCase "Greater2: b < a"           $ greater2 @?= (Right $ FalseVal)
  ,testCase "Greater3: a < a"           $ greater3 @?= (Right $ FalseVal)

  ,testCase "Eq1: 42 == 42"             $ eq1 @?= (Right $ TrueVal)
  ,testCase "Eq2: 42 == -1337"          $ eq2 @?= (Right $ FalseVal)
  ,testCase "Eq3: true == false"        $ eq3 @?= (Right $ FalseVal)
  ,testCase "Eq4: None == [3, \"hej\"]" $ eq4 @?= (Right $ FalseVal)
  ,testCase "Eq5: \"abba\" == \"abbc\"" $ eq5 @?= (Right $ FalseVal)
  ,testCase "Eq6: \"123\" == \"123\""   $ eq6 @?= (Right $ TrueVal)

  ,testCase "In1: empty list"                  $ in1  @?= (Right FalseVal)
  ,testCase "In2: element in list"             $ in2  @?= (Right TrueVal)
  ,testCase "In3: not in list"                 $ in3  @?= (Right FalseVal)
  ,testCase "In4: element in large list"       $ in4  @?= (Right TrueVal)
  ,testCase "In5: element not in large list"   $ in5  @?= (Right FalseVal)
  ,testCase "In6: list in list of lists"       $ in6  @?= (Right TrueVal)
  ,testCase "In7: list of lists not in itself" $ in7  @?= (Right FalseVal)
  ,operateNegTests
  ]

operateTypeError1 = operate Greater FalseVal b
operateTypeError2 = operate In  (ListVal [b]) NoneVal
operateTypeError3 = operate Div (StringVal "hej") (IntVal 0)

operateNegTests :: TestTree
operateNegTests = testGroup "operate negative tests"
  [testCase "operate type mismatch test 1" $
    operateTypeError1 @?= (Left "Type mismatch for Greater")
  ,testCase "operate type mismatch test 2" $
    operateTypeError2 @?= (Left "Type mismatch for In")
  ,testCase "operate type mismatch test 3" $
    operateTypeError3 @?= (Left "Type mismatch for Div")
  ]
