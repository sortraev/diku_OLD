module Utils where

import Types
import Data.List (intersect)



--------------------
--- ATOM HELPERS ---
--------------------
pSpec :: Atom -> PSpec
pSpec (Atom p args) = (p, length args)

args :: Atom -> [Term]
args (Atom _ args') = args'



--------------------
--- MISC UTILITY ---
--------------------
-- composition of a binary and a unary operator.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- are these two lists disjoint??
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint = null .: intersect

-- is xs a subset of ys?
subsetOf :: Eq a => [a] -> [a] -> Bool
subsetOf xs ys = all (`elem` ys) xs

-- given two tuples of lists, pairwisely appends these lists.
combine :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
combine (as, bs) (xs, ys) = (as ++ xs, bs ++ ys)
