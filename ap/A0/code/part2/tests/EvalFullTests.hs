module EvalFullTests where

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitFailure)  -- for when running stand-alone


evalFull' :: Exp -> Integer
evalFull' e = evalFull e initEnv

a = 3
b = 7


---------------------------
-------- constants --------
---------------------------
test_cst0 = ("test_cst0", evalFull' (Cst a) == a)
test_cst1 = ("test_cst1", evalFull' (Cst b) == b)
test_cst2 = ("test_cst2", evalFull' (Cst 0) == 0)


--------------------------
-------- addition --------
--------------------------
test_add0 = ("test_add0", evalFull' (Add (Cst a) (Cst b))      == a+b)
test_add1 = ("test_add1", evalFull' (Add (Cst a) (Cst (-b)))   == a-b)
test_add2 = ("test_add2", evalFull' (Add (Cst (-a)) (Cst (b))) == -a+b)
test_add3 = ("test_add3", evalFull' (Add (Cst (a)) (Cst 0)) == a)
test_add4 = ("test_add4", evalFull' (Add (Cst 0) (Cst a)) == a)


-----------------------------
-------- subtraction --------
-----------------------------
test_sub0 = ("test_sub0", evalFull' (Sub (Cst a) (Cst b))      == a-b)
test_sub1 = ("test_sub1", evalFull' (Sub (Cst a) (Cst (-b)))   == a+b)
test_sub2 = ("test_sub2", evalFull' (Sub (Cst (-a)) (Cst (b))) == -a-b)
test_sub3 = ("test_sub3", evalFull' (Sub (Cst 0) (Cst b)) == -b)
test_sub4 = ("test_sub4", evalFull' (Sub (Cst a) (Cst 0)) == a)


--------------------------------
-------- multiplication --------
--------------------------------
test_mul0 = ("test_mul0", evalFull' (Mul (Cst a) (Cst b)) == a*b)

-- 1 * x == x * 1 == x
test_mul1 = ("test_mul1", evalFull' (Mul (Cst a) (Cst 1)) == a)
test_mul2 = ("test_mul2", evalFull' (Mul (Cst 1) (Cst b)) == b)

-- 0 * x == x * 0 == 0
test_mul3 = ("test_mul3", evalFull' (Mul (Cst a) (Cst 0)) == 0)
test_mul4 = ("test_mul4", evalFull' (Mul (Cst 0) (Cst b)) == 0)


--------------------------
-------- division --------
--------------------------
test_div0 = ("test_div0", evalFull' (Div (Cst a) (Cst b)) == a `div` b)

-- a / 1 = a
test_div1 = ("test_div1", evalFull' (Div (Cst a) (Cst 1)) == a)
-- a / b = 0 for and positive a and b with a < b
test_div2 = ("test_div2", evalFull' (Div (Cst 1) (Cst b)) == 0)
-- 0 / a = 0
test_div3 = ("test_div3", evalFull' (Div (Cst 0) (Cst a)) == 0)
-- 1 / a = -1 for negative a
test_div4 = ("test_div4", evalFull' (Div (Cst 1) (Cst (-a))) == (-1))
-- a / a = 1
test_div5 = ("test_div5", evalFull' (Div (Cst a) (Cst a)) == 1)


------------------------
-------- powers --------
------------------------
test_pow0 = ("test_pow0", evalFull' (Pow (Cst a) (Cst b)) == a ^ b)
test_pow1 = ("test_pow1", evalFull' (Pow (Cst b) (Cst a)) == b ^ a)
test_pow2 = ("test_pow2", evalFull' (Pow (Cst a) (Cst 1)) == a)
test_pow3 = ("test_pow3", evalFull' (Pow (Cst a) (Cst 0)) == 1)
test_pow4 = ("test_pow4", evalFull' (Pow (Cst 0) (Cst 0)) == 1)


-------------------------------
-------- if statements --------
-------------------------------
true  = Cst 42
false = Cst 0

some_branch    = Cst 1337
another_branch = Cst 1338
some_illegal_branch  = Var "unknown_variable"
other_illegal_branch = Div (Cst 42) (Cst 0)

test_if0 = ("test_if0", evalFull' (If true some_branch another_branch)  == 1337)
test_if1 = ("test_if1", evalFull' (If false some_branch another_branch) == 1338)

-- test non-evaluation of illegal branches
test_if2 = ("test_if2", evalFull' (If true some_branch some_illegal_branch)   == 1337)
test_if3 = ("test_if3", evalFull' (If false other_illegal_branch some_branch) == 1337)


---------------------------
-------- variables --------
---------------------------
env0 = extendEnv "a" 42 initEnv
env1 = extendEnv "b" 43 env0

test_var0 = ("test_var0", evalFull (Var "a") env1 == 42)
test_var1 = ("test_var1", evalFull (Var "b") env1 == 43)

-- variable overshadowing.
env2 = extendEnv "a" 100 env1
env3 = extendEnv "b" 43 env2
test_var2 = ("test_var2", evalFull (Var "a") env3 == 100) -- assert that a is overshadowed.
test_var3 = ("test_var3", evalFull (Var "b") env3 == 43)  -- assert that b is intact.


------------------------------
-------- let bindings --------
------------------------------
test_let0 = ("test_let0", evalFull' (Let "a" (Cst 42) (Var "a")) == 42)
test_let1 = ("test_let0", evalFull' (Let "a" (Cst 42) (Var "a")) == 42)

-- testing correct overshadowing: let a = 42 in let a = 43 in a
test_let2 = ("test_let1", evalFull' (Let "a" (Cst 42) (Let "a" (Cst (-42)) (Var "a"))) == (-42))

-- non-evaluation of aux when body is constant
test_let3 = ("test_let2", evalFull' (Let "a" (Div (Cst 42) (Cst 0)) (Cst 42))   == 42)
test_let4 = ("test_let2", evalFull' (Let "a" (Var "unkwown_variable") (Cst 43)) == 43)


---------------------
-------- sum --------
---------------------
-- example from the assignment text
test_sum0 = ("test_sum0", evalFull' (Sum foo (Cst 1) (Cst 4) (Cst 5)) == sum (map (\_ -> 5) [1..4] ))

-- non-trivial sum body
foo = "foo"
x = 3
lo = 9; hi = 17
f = Mul (Var foo) (Cst x)
test_sum1 = ("test_sum1", evalFull' (Sum foo (Cst lo) (Cst hi) f) == sum (map (*x) [lo..hi]))

-- sum over an empty range
lo' = 10; hi' = 0
test_sum2 = ("test_sum2", evalFull' (Sum foo (Cst lo') (Cst hi') f) == sum (map (*x) [lo'..hi']))
test_sum3 = ("test_sum3", evalFull' (Sum foo (Cst 100) (Cst 0) (Div (Cst 42) (Cst 0))) == 0)


tests :: [(String, Bool)]
tests = [test_cst0, test_cst1, test_cst2,
         test_add0, test_add1, test_add2, test_add3, test_add4,
         test_sub0, test_sub1, test_sub2, test_sub3, test_sub4,
         test_mul0, test_mul1, test_mul2, test_mul3, test_mul4,
         test_div0, test_div1, test_div2, test_div3, test_div4, test_div5,
         test_pow0, test_pow1, test_pow2, test_pow3, test_pow4,
         test_if0,  test_if1,  test_if2,  test_if3,
         test_var0, test_var1, test_var2, test_var3,
         test_let0, test_let1, test_let2, test_let3, test_let4,
         test_sum0, test_sum1, test_sum2, test_sum3]



run :: IO ()
run =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All evalFull tests passed!"
       _ ->  do putStrLn $ "Failed evalFull tests: " ++ intercalate ", " failed
                exitFailure
