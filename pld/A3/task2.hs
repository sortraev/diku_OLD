-- forall a b . a -> b -> c
-- dingo :: Eq a => a -> [a] -> Bool
dingo x (y:ys) = x == y || dingo x ys
dingo x _      = False
