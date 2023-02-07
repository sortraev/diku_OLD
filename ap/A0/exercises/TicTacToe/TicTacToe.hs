{-
  Code for tic-tac-toe games

  Date: Sep 2, 2016
  Author: Ken Friis Larsen <kflarsen@diku.dk>, Maya Saietz <mayasaietz@gmail.com>
-}

module TicTacToe where

data Player = Cross | Nought
            deriving (Eq, Show)

data Cell = Move Player | Empty
          deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

dims = 3

emptyBoard :: Board
emptyBoard = replicate dims (replicate dims Empty)

type Position = (Int, Int)

-- You should probably take a good look at this function until you
-- understand it. Keep in mind that it does not check whether the move
-- is valid.
move :: Position -> Board -> Player -> Board
move (x, y) board player = rowsBefore ++ (changed : rowsAfter)
  where (rowsBefore, toBeChanged : rowsAfter) = splitAt x board
        changed = cellsBefore ++ (newCell : cellsAfter)
        (cellsBefore, _old : cellsAfter) = splitAt y toBeChanged
        newCell = Move player

-- Type for representing the state of a game. That is, which should
-- take the next move, and what does the board look like.
type GameState = (Player, Board)

startState :: GameState
startState = (Cross, emptyBoard)

-- Hint: You already have the move function, defined above, to do most
-- of the legwork.
makeMove :: Position -> GameState -> GameState
makeMove pos current_state@(current_player, board) =
  if validMove pos current_state then
    let next_player = if current_player == Cross then Nought else Cross
        next_state  = move pos board current_player
    in (next_player, next_state)
  else current_state

validMove :: Position -> GameState -> Bool
validMove (x, y) (_, board) =
  x >= 0 && x < dims &&
  y >= 0 && y < dims &&
  ((board !! x) !! y) == Empty

allMoves :: [Position]
allMoves = [(x, y) | x <- [0 .. dims - 1], y <- [0 .. dims - 1]]

allValidMoves :: GameState -> [Position]
allValidMoves current_state = filter (\pos -> validMove pos current_state) allMoves

-- The type for representing game trees.  Each node have a game state
-- and a list of *valid* moves leading to (sub-) game trees
data GameTree = Node GameState [(Position, GameTree)]

-- Grow the game tree starting in a given state
makeTree :: GameState -> GameTree
makeTree state = Node state sub_games
  where valid_moves  = allValidMoves state
        valid_next_states = map (\move -> makeMove move state) valid_moves
        sub_games    = zip valid_moves $ map makeTree valid_next_states


-- Return all game states in a game tree, duplicates are allowed
allNodes :: GameTree -> [GameState]
allNodes (Node gs subs) = gs : concatMap (allNodes . snd) subs
