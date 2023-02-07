module Warmup where

----------------- Move -----------------
type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)
move West  (x, y) = (x-1, y)

moves :: [Direction] -> Pos -> Pos
moves dirs pos = foldr move pos dirs


----------------- Nat -----------------
data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add, mult :: Nat -> Nat -> Nat
add (Succ x) y = add x (Succ y)
add _        y = y
mult a (Succ b) = add a (mult a b)
mult _ _        = Zero

int2nat :: Int -> Nat
int2nat n
  | n < 0     = error "Not a natural number!"
  | n == 0    = Zero
  | otherwise = Succ (int2nat (n - 1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = nat2int x + 1


----------------- Tree -----------------
data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x t@(Node key t1 t2)
  | x == key  = t
  | x < key   = Node key (insert x t1) t2
  | otherwise = Node key t1 (insert x t2)

insert x _ = Node x Leaf Leaf
