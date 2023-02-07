data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)


add :: Nat -> Nat -> Nat
add (Succ x) y = add x (Succ y)
add _        y = y


-- addSmart :: Nat -> Nat -> Nat
-- addSmart sx@(Succ x) sy@(Succ y)
--   | x <= y    = addRight sx sy
--   | otherwise = addLeft  sx sy
--
-- addRight :: Nat -> Nat -> Nat
-- addRight (Succ x) y = addRight x (Succ y)
-- addRight _        y = y
--
-- addLeft :: Nat -> Nat -> Nat
-- addLeft x (Succ y) = addLeft (Succ x) y
-- addLeft x        _ = x


mul :: Nat -> Nat -> Nat
mul a (Succ b) = add a (mul a b)
mul _ _        = Zero


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = if n < 0 then error "Not a natural number!"
                     else Succ (int2nat (n-1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = (nat2int x) + 1

-- nat2int :: Nat -> Int
-- nat2int nat = nat2int' nat 0
--
-- nat2int' :: Nat -> Int -> Int
-- nat2int' Zero       acc = acc
-- nat2int' (Succ nat) acc = nat2int' nat (acc+1)
--
--
-- int2nat :: Int -> Nat
-- int2nat int = int2nat' int Zero
--
-- int2nat' :: Int -> Nat -> Nat
-- int2nat' 0   nat = nat
-- int2nat' acc nat = int2nat' (acc-1) (Succ nat)


-- helpers
add' :: Int -> Int -> Int
add' x y = nat2int $ add (int2nat x) (int2nat y)

mul' :: Int -> Int -> Int
mul' x y = nat2int $ mul (int2nat x) (int2nat y)






-- test utility

assert :: Bool -> ()
assert False  = error "test failed"
assert _      = ()

_show :: Show a => a -> String
_show x = show x ++ "\n"


-- tests

_ = assert (int2nat 0 == Zero)
_ = assert (int2nat 2 == (Succ (Succ Zero)))

_ = assert (nat2int Zero == 0)
_ = assert (nat2int (Succ (Succ (Succ Zero))) == 3)

_ = assert ((nat2int (int2nat 3718)) == 3718)


a = 17
b = 38

nat_a = int2nat a
nat_b = int2nat b

_ = assert ((add nat_a nat_b) == (int2nat (a+b)))
_ = assert ((add nat_a Zero) == nat_a)
_ = assert ((add Zero nat_b) == nat_b)

_ = assert ((mul nat_a nat_b) == (int2nat (a*b)))
_ = assert ((mul nat_a Zero) == Zero)
_ = assert ((mul Zero nat_b) == Zero)

_ = assert ((mul nat_a (Succ Zero)) == nat_a)
_ = assert ((mul (Succ Zero) nat_b) == nat_b)


main = putStrLn $ "success"
