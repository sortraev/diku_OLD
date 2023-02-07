module EvalErrTests where
--
import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitFailure)  -- for when running stand-alone


evalErr' :: Exp -> Either ArithError Integer
evalErr' e = evalErr e initEnv


a = 3
b = 7

badVar = Var "foo"
zeroDiv = Div (Cst 42) (Cst 0)
negPower = Pow (Cst 42) (Cst (-42))


---------------------------
-------- constants --------
---------------------------
test_cst0 = ("test_cst0", evalErr' (Cst a) == (Right a))
test_cst1 = ("test_cst1", evalErr' (Cst b) == (Right b))
test_cst2 = ("test_cst2", evalErr' (Cst 0) == (Right 0))


--------------------------
-------- addition --------
--------------------------
test_add0 = ("test_add0", evalErr' (Add (Cst a) (Cst b))      == (Right (a+b)))
test_add1 = ("test_add1", evalErr' (Add (Cst a) (Cst (-b)))   == (Right (a-b)))
test_add2 = ("test_add2", evalErr' (Add (Cst (-a)) (Cst (b))) == (Right $ -a+b))

negtest_add0 = ("negtest_add0", evalErr' (Add negPower (Cst b)) == Left ENegPower)
negtest_add1 = ("negtest_add1", evalErr' (Add (Cst a) zeroDiv)  == Left EDivZero)
negtest_add2 = ("negtest_add2", evalErr' (Add badVar zeroDiv)   == Left (EBadVar "foo"))

-----------------------------
-------- subtraction --------
-----------------------------
test_sub0 = ("test_sub0", evalErr' (Sub (Cst a) (Cst b))      == Right (a-b))
test_sub1 = ("test_sub1", evalErr' (Sub (Cst a) (Cst (-b)))   == Right (a+b))
test_sub2 = ("test_sub2", evalErr' (Sub (Cst (-a)) (Cst (b))) == Right (-a-b))
              
negtest_sub0 = ("negtest_sub0", evalErr' (Sub zeroDiv (Cst b))  == Left EDivZero)
negtest_sub1 = ("negtest_sub1", evalErr' (Sub (Cst a) negPower) == Left ENegPower)
negtest_sub2 = ("negtest_sub2", evalErr' (Sub badVar zeroDiv)   == Left (EBadVar "foo"))

--------------------------------
-------- multiplication --------
--------------------------------
test_mul0 = ("test_mul0", evalErr' (Mul (Cst a) (Cst b)) == Right (a*b))

-- 1 * x == x * 1 == x
test_mul1 = ("test_mul1", evalErr' (Mul (Cst a) (Cst 1)) == Right (a))
test_mul2 = ("test_mul2", evalErr' (Mul (Cst 1) (Cst b)) == Right (b))

-- 0 * x == x * 0 == 0
test_mul3 = ("test_mul3", evalErr' (Mul (Cst a) (Cst 0)) == Right (0))
test_mul4 = ("test_mul4", evalErr' (Mul (Cst 0) (Cst b)) == Right (0))


-- errors in operands
negtest_mul0 = ("negtest_mul0", evalErr' (Mul zeroDiv (Cst b))  == Left EDivZero)
negtest_mul1 = ("negtest_mul1", evalErr' (Mul (Cst a) negPower) == Left ENegPower)
negtest_mul2 = ("negtest_mul2", evalErr' (Mul zeroDiv badVar)   == Left EDivZero)

--------------------------
-------- division --------
--------------------------
test_div0 = ("test_div0", evalErr' (Div (Cst a) (Cst b)) == Right (a `quot` b))
test_div1 = ("test_div1", evalErr' (Div (Cst a) (Cst 1)) == Right (a))
test_div2 = ("test_div2", evalErr' (Div (Cst 1) (Cst b)) == Right (0))
test_div3 = ("test_div3", evalErr' (Div (Cst 0) (Cst a)) == Right (0))


-- zero div error
negtest_div0 = ("negtest_div0", evalErr' zeroDiv                == Left EDivZero)

-- errors in operands
negtest_div1 = ("negtest_div1", evalErr' (Div negPower (Cst 3)) == Left ENegPower)
negtest_div2 = ("negtest_div2", evalErr' (Div (Cst 6) badVar)   == Left (EBadVar "foo"))
negtest_div3 = ("negtest_div3", evalErr' (Div badVar negPower)  == Left ENegPower)



------------------------
-------- powers --------
------------------------
test_pow0 = ("test_pow0", evalErr' (Pow (Cst a) (Cst b)) == Right (a ^ b))
test_pow1 = ("test_pow1", evalErr' (Pow (Cst b) (Cst a)) == Right (b ^ a))
test_pow2 = ("test_pow2", evalErr' (Pow (Cst a) (Cst 1)) == Right (a))
test_pow3 = ("test_pow3", evalErr' (Pow (Cst a) (Cst 0)) == Right (1))
test_pow4 = ("test_pow4", evalErr' (Pow (Cst 0) (Cst 0)) == Right (1))

-- negative exponent
negtest_pow0 = ("negtest_pow0", evalErr' (Pow (Cst a) (Cst (-100))) == Left ENegPower)

-- errors in operands
negtest_pow1 = ("negtest_pow1", evalErr' (Pow zeroDiv (Cst a)) == Left EDivZero)
negtest_pow2 = ("negtest_pow2", evalErr' (Pow (Cst a) badVar)  == Left (EBadVar "foo"))

-- assert that errors in the base are communicated before errors in the exponent
negtest_pow3 = ("negtest_pow3", evalErr' (Pow zeroDiv negPower) == Left ENegPower)

-- assert that if x contains error, then x^0 produces that error and does not
-- evaluate to 1.
negtest_pow4 = ("negtest_pow4", evalErr' (Pow badVar (Cst 0)) == Left (EBadVar "foo"))

-------------------------------
-------- if statements --------
-------------------------------
true  = Cst 42
false = Cst 0

some_branch    = Cst 1337
another_branch = Cst 1338

test_if0 = ("test_if0", evalErr' (If true some_branch another_branch)  == Right 1337)
test_if1 = ("test_if1", evalErr' (If false some_branch another_branch) == Right 1338)

-- test non-evaluation of illegal branches
test_if2 = ("test_if2", evalErr' (If true some_branch badVar)   == Right 1337)
test_if3 = ("test_if3", evalErr' (If false zeroDiv some_branch) == Right 1337)

-- error in condition
negtest_if0 = ("negtest_if0", evalErr' (If zeroDiv some_branch another_branch)  == Left EDivZero)
-- error in true branch
negtest_if1 = ("negtest_if1", evalErr' (If true negPower zeroDiv) == Left ENegPower)
-- error in false branch
negtest_if2 = ("negtest_if2", evalErr' (If zeroDiv some_branch badVar) == Left (EBadVar "foo"))


---------------------------
-------- variables --------
---------------------------
env0 = extendEnv "a" 42 initEnv
env1 = extendEnv "b" 43 env0

test_var0 = ("test_var0", evalErr (Var "a") env0 == Right 42)
test_var1 = ("test_var1", evalErr (Var "b") env1 == Right 43)

-- variable overshadowing.
env2 = extendEnv "a" 100 env1
test_var2 = ("test_var2", evalErr (Var "a") env2 == Right 100) -- assert that a is overshadowed.
test_var3 = ("test_var3", evalErr (Var "b") env2 == Right 43)  -- assert that b is intact.

negtest_var0 = ("negtest_var0", evalErr badVar initEnv == Left (EBadVar "foo"))
negtest_var1 = ("negtest_var1", evalErr badVar env1    == Left (EBadVar "foo"))


------------------------------
-------- let bindings --------
------------------------------
test_let0 = ("test_let0", evalErr' (Let "a" (Cst 42) (Var "a")) == Right 42)
test_let1 = ("test_let1", evalErr' (Let "a" (Cst 42) (Var "a")) == Right 42)

-- testing correct overshadowing: let a = 42 in let a = 43 in a
test_let2 = ("test_let2", evalErr' (Let "a" (Cst 42) (Let "a" (Cst (-42)) (Var "a"))) == Right (-42))

-- error in binding value (even though body is constant; see report)
negtest_let0 = ("negtest_let0", evalErr' (Let "a" zeroDiv  (Cst 42)) == Left EDivZero)
negtest_let1 = ("negtest_let1", evalErr' (Let "a" negPower (Cst 43)) == Left ENegPower)

-- error in let body
negtest_let2 = ("negtest_let2", evalErr' (Let "a" (Cst 3) zeroDiv) == Left EDivZero)


---------------------
-------- sum --------
---------------------
-- example from the assignment text

-- non-trivial sum body
bar = "bar"
x = 3
lo = 9; hi = 17
f = Mul (Var bar) (Cst x)
g = Div (Cst 42) (Var bar)

test_sum0 = ("test_sum0", evalErr' (Sum bar (Cst 1) (Cst 4) (Cst 5)) == Right (sum (map (\_ -> 5) [1..4])))
test_sum1 = ("test_sum1", evalErr' (Sum bar (Cst lo) (Cst hi) f)   == Right (sum (map (*x) [lo..hi])))

-- sum over an empty range
lo' = 10; hi' = 0
test_sum2 = ("test_sum2", evalErr' (Sum bar (Cst lo') (Cst hi') f) == Right (sum (map (*x) [lo'..hi'])))
test_sum3 = ("test_sum3", evalErr' (Sum bar (Cst 100) (Cst 0) zeroDiv) == Right 0)

-- tests of all combinations of errors in summation bounds and body
negtest_sum0 = ("negtest_sum0", evalErr' (Sum bar negPower (Cst hi') (Cst 5))  == Left ENegPower)
negtest_sum1 = ("negtest_sum1", evalErr' (Sum bar (Cst lo') badVar (Cst 5))    == Left (EBadVar "foo"))
negtest_sum2 = ("negtest_sum2", evalErr' (Sum bar (Cst lo') (Cst hi') zeroDiv) == Left EDivZero)
negtest_sum3 = ("negtest_sum3", evalErr' (Sum bar negPower badVar (Cst 5))     == Left ENegPower)
negtest_sum4 = ("negtest_sum4", evalErr' (Sum bar (Cst lo') badVar zeroDiv )   == Left (EBadVar "foo"))
negtest_sum5 = ("negtest_sum5", evalErr' (Sum bar zeroDiv (Cst hi') (Var "z")) == Left EDivZero )
negtest_sum6 = ("negtest_sum6", evalErr' (Sum bar zeroDiv (Cst hi') negPower)  == Left EDivZero )
negtest_sum7 = ("negtest_sum7", evalErr' (Sum bar badVar negPower zeroDiv)     == Left (EBadVar "foo"))

-- error in only some iterations of summation body
negtest_sum8 = ("negtest_sum8", evalErr' (Sum bar (Cst (-2)) (Cst 2) g) == Left EDivZero)

tests :: [(String, Bool)]
tests = [test_cst0, test_cst1,
         test_add0, test_add1, test_add2,
         negtest_add0, negtest_add1, negtest_add2,
         test_sub0, test_sub1, test_sub2,
         negtest_sub0, negtest_sub1, negtest_sub2,
         test_mul0, test_mul1, test_mul2, test_mul3, test_mul4,
         negtest_mul0, negtest_mul1, negtest_mul2,
         test_div0, test_div1, test_div2, test_div3,
         negtest_div0, negtest_div1, negtest_div2, negtest_div3,
         test_pow0, test_pow1, test_pow2, test_pow3, test_pow4,
         negtest_pow0, negtest_pow1, negtest_pow2, negtest_pow3, negtest_pow4,
         test_if0,  test_if1,  test_if2,  test_if3,
         test_var0, test_var1, test_var2, test_var3,
         negtest_var0, negtest_var1,
         test_let0, test_let1, test_let2,
         negtest_let0, negtest_let1, negtest_let2,
         test_sum0, test_sum1, test_sum2, test_sum3,
         negtest_sum3, negtest_sum4, negtest_sum5, negtest_sum6, negtest_sum7]



run :: IO ()
run =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All evalErr tests passed!"
       _ -> do putStrLn $ "Failed evalErr tests: " ++ intercalate ", " failed
               exitFailure
