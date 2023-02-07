module EvalTests where

import Test.Tasty
import Test.Tasty.HUnit

import BoaAST
import BoaInterp

----------------------
----- eval tests -----
----------------------
range_x = Call "range" $ map (Const . IntVal) [-50, 51]
range_z = Call "range" $ map (Const . IntVal) [1, 501, 100]
from = Const $ IntVal 127
to   = Const $ IntVal $ -208
step = Const $ IntVal $ -24
range_y = Call "range" $ map (Const . IntVal) [127, -208, -24]

compr1 = Compr (Var "z") [CCFor "z" range_z, CCIf (Oper Greater (Var "z") (Const $ IntVal 500))]
compr1_expected = ListVal []

compr2 = Compr (Var "z") [CCIf (Const TrueVal), CCFor "z" range_z]
compr2_expected = ListVal $ map IntVal [1, 101..500]

compr3 = Compr (Var "x") [CCFor "x" range_x, CCIf (Not (Oper Mod (Var "x") (Const $ IntVal 7)))]
compr3_expected = ListVal $ map IntVal [x | x <- [-50..50], mod x 7 == 0]


compr4_cond = Oper Greater (Oper Mod (Oper Plus (Var "x") (Var "y")) (Const $ IntVal 27))
                    (Oper Plus (Var "y") (Const (IntVal 13)))
compr4 = Compr (Oper Div (Oper Times (Var "x") (Var "y")) (Var "z"))
               [CCFor "x" range_x, CCFor "y" range_y, CCIf compr4_cond, CCFor "z" range_z]
compr4_expected = ListVal $ map IntVal [div (x * y) z | x <- [-50..50],
                                                        y <- [127, 103..(-208)],
                                                        mod (x + y) 27 > y + 13,
                                                        z <- [1, 101..500]]

comprTests :: TestTree
comprTests = testGroup "Compr tests"
  [
   testCase "Compr1: empty result" $
     runComp (eval compr1) [] @?= (Right compr1_expected, [])

  ,testCase "Compr2: first clause an if" $
     runComp (eval compr2) [] @?= (Right compr2_expected, [])

  ,testCase "Compr3: simple comprehension" $
     runComp (eval compr3) [] @?= (Right compr3_expected, [])

  ,testCase "Compr4: contrived comprehension" $
     runComp (eval compr4) [] @?= (Right compr4_expected, [])
  ]

compr5 = Compr (Var "x") [CCFor "x" (Const $ IntVal 4)]

comprNegTests :: TestTree
comprNegTests = testGroup "Compr negative tests"
  [
   testCase "Compr5: non-list in generator" $ runComp (eval compr5) []
     @?= (Left $ EBadArg "Compr error: generator expects list expression", [])
  ]


evalTests :: TestTree
evalTests = testGroup "eval tests"
  [
  comprTests
 ,comprNegTests
  ]
