module ApplyTests where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaInterp

-----------------------
----- apply tests -----
-----------------------
--- print tests ---
emptyPrint = apply "print" []

printTests :: TestTree
printTests = testGroup "print tests"
  [testCase "print(), empty call" $ runComp emptyPrint [] @?= (Right NoneVal, [""])
  ]


--- range tests ---

rangeOneArg    = apply "range" [IntVal 219]
rangeTwoArgs   = apply "range" [IntVal (-427), IntVal 50]
rangeThreeArgs = apply "range" [IntVal 273, IntVal (-100), IntVal (-20)]
rangeEmpty     = apply "range" [IntVal 3, IntVal 4, IntVal (-1)]

rangeTests :: TestTree
rangeTests = testGroup "range() positive tests"
  [
   testCase "range: one arg"    $ runComp rangeOneArg [] @?=
     (Right (ListVal $ map IntVal [0..218]), [])
  ,testCase "range: two args"   $ runComp rangeTwoArgs [] @?=
     (Right (ListVal $ map IntVal [-427..49]), [])
  ,testCase "range: three args" $ runComp rangeThreeArgs [] @?=
     (Right (ListVal $ map IntVal [273, 273-20..(-100)]), []) 
  ,testCase "range: hi > lo, step < 0" $ runComp rangeEmpty [] @?=
     (Right (ListVal []), []) 
  ]

rangeZeroArgs = apply "range" []
rangeSeventeenArgs = apply "range" $ map IntVal [4..20]

rangeTypeErr1 = apply "range" [NoneVal]
rangeTypeErr2 = apply "range" [IntVal 1, TrueVal]
rangeTypeErr3 = apply "range" [IntVal 1, StringVal "hej", IntVal 3]

rangeNegTests :: TestTree
rangeNegTests = testGroup "range negative tests"
  [testCase "range: zero args" $ runComp rangeZeroArgs [] @?=
     (Left $ EBadArg "range expects 1-3 arguments, got 0", []) 
  ,testCase "range: too many args" $ runComp rangeSeventeenArgs [] @?=
     (Left $ EBadArg "range expects 1-3 arguments, got 17", []) 

  ,testCase "range: type error test 1" $ runComp rangeTypeErr1 [] @?=
     (Left $ EBadArg "range expects integer arguments only", []) 
  ,testCase "range: type error test 2" $ runComp rangeTypeErr2 [] @?=
     (Left $ EBadArg "range expects integer arguments only", []) 
  ,testCase "range: type error test 3" $ runComp rangeTypeErr3 [] @?=
     (Left $ EBadArg "range expects integer arguments only", [])
  ]


--- apply, unknown functions ---
unknownFun1 = apply "myFun" []
unknownFun2 = apply "main()" [IntVal 2, ListVal [StringVal "argv"]]

applyUnknownFunTests :: TestTree
applyUnknownFunTests = testGroup "Negative apply tests"
  [testCase "apply, unknown function 1" $ runComp unknownFun1 [] @?= (Left $ EBadFun "myFun", [])
  ,testCase "apply, unknown function 2" $ runComp unknownFun2 [] @?= (Left $ EBadFun "main()", [])
  ]

applyTests :: TestTree
applyTests = testGroup "apply tests"
  -- printTests ++ rangeTests ++ rangeNegTests ++ applyUnknownFunTests
  [
   printTests
  ,rangeTests
  ,rangeNegTests
  ,applyUnknownFunTests
  ]


