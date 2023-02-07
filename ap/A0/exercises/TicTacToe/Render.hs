

module Render where

import TicTacToe

showCell :: Cell -> Char
showCell (Move Cross)  = 'X'
showCell (Move Nought) = 'O'
showCell _             = ' '

readCell :: Char -> Cell
readCell 'X' = Move Cross
readCell 'O' = Move Nought
readCell _   = Empty

showState :: GameState -> [Char]
showState = showBoard . snd

-- TODO: readState

showBoard :: Board -> [Char]
showBoard = unlines . map (map showCell)

readBoard :: [Char] -> Board
readBoard = (map (map readCell)) . lines

initBoardStr :: [Char]
initBoardStr = "   \n   \n   \n"

initBoard :: Board
initBoard = readBoard initBoardStr

main = putStrLn $ showBoard initBoard
