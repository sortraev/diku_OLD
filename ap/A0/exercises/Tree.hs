
module Tree where

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree

insert x original@(Node key left right)
  | x == key  = original
  | x < key   = Node key (insert x left) right
  | otherwise = Node key left (insert x right)

insert x _ = Node x Leaf Leaf
