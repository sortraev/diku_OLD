
module Size where

import Tree


class Sizeable t where
  size :: t -> Int


instance Sizeable Int where
  size _ = 1

instance Sizeable Char where
  size _ = 1


instance Sizeable a => Sizeable [a] where
  size xs = (sum (map size xs)) + (length xs) + 1


-- tuples and trees
instance (Sizeable a, Sizeable b) => Sizeable (a, b) where
  size (x, y) = size x + size y + 1

instance Sizeable Tree where
  size (Node key left right) = size key + size left + size right + 1
  size Leaf = 1
