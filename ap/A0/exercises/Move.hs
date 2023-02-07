module Move where


type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos

move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)
move West  (x, y) = (x-1, y)

moves :: [Direction] -> Pos -> Pos
moves dirs pos = foldr move pos dirs
