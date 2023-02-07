module EvalSimpleTests where

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitFailure)


a = -3
b = 7

---------------------------
-------- constants --------
---------------------------
test_cst0 = ("test_cst0", evalSimple (Cst a) == a)
test_cst1 = ("test_cst1", evalSimple (Cst b) == b)
test_cst2 = ("test_cst2", evalSimple (Cst 0) == 0)


--------------------------
-------- addition --------
--------------------------
test_add0 = ("test_add0", evalSimple (Add (Cst a) (Cst b)) == a+b)
test_add1 = ("test_add1", evalSimple (Add (Cst a) (Cst (-b))) == a-b)
test_add2 = ("test_add2", evalSimple (Add (Cst (-a)) (Cst (b))) == -a+b)
test_add3 = ("test_add3", evalSimple (Add (Cst (a)) (Cst 0)) == a)


-----------------------------
-------- subtraction --------
-----------------------------
test_sub0 = ("test_sub0", evalSimple (Sub (Cst a) (Cst b)) == a-b)
test_sub1 = ("test_sub1", evalSimple (Sub (Cst a) (Cst (-b))) == a+b)
test_sub2 = ("test_sub2", evalSimple (Sub (Cst (-a)) (Cst (b))) == -a-b)
test_sub3 = ("test_sub3", evalSimple (Sub (Cst 0) (Cst b)) == -b)
test_sub4 = ("test_sub4", evalSimple (Sub (Cst a) (Cst 0)) == a)


--------------------------------
-------- multiplication --------
--------------------------------
test_mul0 = ("test_mul0", evalSimple (Mul (Cst a) (Cst b)) == a*b)

-- 1 * x == x * 1 == x
test_mul1 = ("test_mul1", evalSimple (Mul (Cst a) (Cst 1)) == a)
test_mul2 = ("test_mul2", evalSimple (Mul (Cst 1) (Cst b)) == b)

-- 0 * x == x * 0 == 0
test_mul3 = ("test_mul3", evalSimple (Mul (Cst a) (Cst 0)) == 0)
test_mul4 = ("test_mul4", evalSimple (Mul (Cst 0) (Cst b)) == 0)


--------------------------
-------- division --------
--------------------------
test_div0 = ("test_div0", evalSimple (Div (Cst a) (Cst b)) == a `div` b)
test_div1 = ("test_div1", evalSimple (Div (Cst a) (Cst 1)) == a)
test_div2 = ("test_div2", evalSimple (Div (Cst 1) (Cst b)) == 0)
test_div3 = ("test_div3", evalSimple (Div (Cst 0) (Cst a)) == 0)
test_div4 = ("test_div4", evalSimple (Div (Cst 1) (Cst a)) == (-1))
test_div5 = ("test_div5", evalSimple (Div (Cst a) (Cst a)) == 1)


------------------------
-------- powers --------
------------------------
c = 37
d = 12
e = -271
test_pow0 = ("test_pow0", evalSimple (Pow (Cst c) (Cst d)) == c ^ d)
test_pow1 = ("test_pow0", evalSimple (Pow (Cst e) (Cst c)) == e ^ c) -- negative base
test_pow2 = ("test_pow2", evalSimple (Pow (Cst e) (Cst 1)) == e)
test_pow3 = ("test_pow3", evalSimple (Pow (Cst e) (Cst 0)) == 1)
test_pow4 = ("test_pow4", evalSimple (Pow (Cst 0) (Cst 0)) == 1)
test_pow5 = ("test_pow5", evalSimple (Pow (Cst c) (Cst 0)) == 1)


tests :: [(String, Bool)]
tests = [test_cst0,
         test_add0, test_add1, test_add2, test_add3,
         test_sub0, test_sub1, test_sub2, test_sub3, test_sub4,
         test_mul0, test_mul1, test_mul2, test_mul3, test_mul4,
         test_div0, test_div1, test_div2, test_div3, test_div4, test_div5,
         test_pow0, test_pow1, test_pow2, test_pow3, test_pow4, test_pow5]


run :: IO ()
run =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All evalSimple tests passed!"
       _ -> do putStrLn $ "Failed evalSimple tests: " ++ intercalate ", " failed
               exitFailure
