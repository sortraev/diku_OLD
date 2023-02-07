-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst val  ) = "(" ++ show val ++ ")"
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ "/" ++ showExp e2 ++ ")"
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ "^" ++ showExp e2 ++ ")"
showExp _ = error "Not printable"


evalSimple :: Exp -> Integer
evalSimple (Cst val) = val
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2
evalSimple (Div e1 e2) =
  if e2_val /= 0 then evalSimple e1 `div` e2_val
                 else error "Zero-division error"
  where e2_val = evalSimple e2
evalSimple (Pow e1 e2) =
  if e2_val >= 0 then seq e1_val (e1_val ^ e2_val) -- force strict evaluation of e1_val.
                 else error "Negative exponent in power"
  where e1_val = evalSimple e1
        e2_val = evalSimple e2
evalSimple _ = error "Expression not simple"


extendEnv :: VName -> Integer -> Env -> Env
extendEnv new_var val env v = if v == new_var then Just val else env v

evalFull :: Exp -> Env -> Integer
evalFull (Cst val)   _   = val
evalFull (Add e1 e2) env = evalFull e1 env + evalFull e2 env
evalFull (Sub e1 e2) env = evalFull e1 env - evalFull e2 env
evalFull (Mul e1 e2) env = evalFull e1 env * evalFull e2 env
evalFull (Div e1 e2) env =
  if e2_val /= 0 then evalFull e1 env `div` e2_val
                 else error "Zero division error"
  where e2_val = evalFull e2 env
evalFull (Pow e1 e2) env =
  if e2_val >= 0 then seq e1_val (e1_val ^ e2_val) -- force strict evaluation of e1_val.
                 else error "Negative exponent in power"
  where e1_val = evalFull e1 env
        e2_val = evalFull e2 env

evalFull (If e1 e2 e3) env = -- evaluate condition; then (only) appropriate branch.
  evalFull (if evalFull e1 env /= 0 then e2 else e3) env

evalFull (Var v) env =
  case env v of
    Just val -> val
    Nothing  -> error ("Unknown variable \"" ++ v ++ "\"")

evalFull (Let v e1 e2) env =
  evalFull e2 $ extendEnv v (evalFull e1 env) env -- bind v = e1 and evaluate e2.

evalFull (Sum v e1 e2 e3) env =
  sum $ map sum_body range -- evaluate all iterations of body and sum them up.
  where range      = [evalFull e1 env .. evalFull e2 env]
        sum_body e = evalFull e3 $ extendEnv v e env


evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst val)   _   = Right val
evalErr (Add e1 e2) env = (+) <$> evalErr e1 env <*> evalErr e2 env
evalErr (Sub e1 e2) env = (-) <$> evalErr e1 env <*> evalErr e2 env
evalErr (Mul e1 e2) env = (*) <$> evalErr e1 env <*> evalErr e2 env

-- for Div and Pow, we catch errors in the second operand first to be
-- consistent with evalSimple and evalFull. However, if x contains an error,
-- then x^0 will still evaluate to that error and not 1.
evalErr (Div e1 e2) env =
  evalErr e2 env >>= \m -> evalErr e1 env >>= \n ->
    if m /= 0 then Right (div n m) else Left EDivZero
evalErr (Pow e1 e2) env =
  evalErr e2 env >>= \y -> evalErr e1 env >>= \x ->
    if y >= 0 then Right (x ^ y) else Left ENegPower

evalErr (Var v) env =
  case env v of
    Just val -> Right val
    Nothing  -> Left (EBadVar v)

evalErr (Let v e1 e2) env =
  evalErr e1 env >>= \aux -> evalErr e2 (extendEnv v aux env)

evalErr (If e1 e2 e3) env = -- evaluate condition; then (only) appropriate branch.
  evalErr e1 env >>= \cond -> evalErr (if cond /= 0 then e2 else e3) env

evalErr (Sum v e1 e2 e3) env =
  evalErr e1 env >>= \lo -> evalErr e2 env >>= \hi ->
    sum' $ map (\i -> evalErr e3 (extendEnv v i env)) [lo..hi] -- evaluate all iterations
  where sum' = foldl ((<*>) . fmap (+)) (Right 0)              -- of body and sum them up.


-- evalLazy
type Env' = VName -> Either ArithError Integer

extendEnv' :: VName -> Either ArithError Integer -> Env' -> Env'
extendEnv' new_var val old_env = \v -> if v == new_var then val else old_env v

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy e env = evalLazy' e (\v -> case env v of  -- for Env compability
                                      Just val -> Right val
                                      _        -> Left (EBadVar v))

evalLazy' :: Exp -> Env' -> Either ArithError Integer
evalLazy' (Var v) env = env v -- even simpler now that Env' has same return type as evalLazy'

evalLazy' (Let v e1 e2) env =
  evalLazy' e2 $ extendEnv' v (evalLazy' e1 env) env  -- env is not extended until necessary

evalLazy' (Sum v e1 e2 e3) env =
  evalLazy' e1 env >>= \lo -> evalLazy' e2 env >>= \hi ->
    sum' $ map (\i -> evalLazy' e3 (extendEnv' v (Right i) env)) [lo..hi] -- evaluate all iterations
  where sum' = foldl ((<*>) . fmap (+)) (Right 0)                         -- of body and sum them up.

-- the remaining cases are copy+pasted from our solution to evalErr
evalLazy' (Cst val)   _   = Right val
evalLazy' (Add e1 e2) env = (+) <$> evalLazy' e1 env <*> evalLazy' e2 env
evalLazy' (Sub e1 e2) env = (-) <$> evalLazy' e1 env <*> evalLazy' e2 env
evalLazy' (Mul e1 e2) env = (*) <$> evalLazy' e1 env <*> evalLazy' e2 env
evalLazy' (Div e1 e2) env =
  evalLazy' e2 env >>= \m -> evalLazy' e1 env >>= \n ->
    if m /= 0 then Right (div n m) else Left EDivZero
evalLazy' (Pow e1 e2) env =
  evalLazy' e2 env >>= \y -> evalLazy' e1 env >>= \x ->
    if y >= 0 then Right (x ^ y) else Left ENegPower
evalLazy' (If e1 e2 e3) env =
  evalLazy' e1 env >>= \cond -> evalLazy' (if cond /= 0 then e2 else e3) env


evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = evalErr


showCompact :: Exp -> String
showCompact = undefined
