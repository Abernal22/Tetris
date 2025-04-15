module Board
    ( Board
    , Cell
    , emptyBoard
    , drawBoard
    , updateBoardWithTetromino
    ) where

import Tetromino (Tetromino, tetrominoBlocks)

type Cell = Maybe Char
type Board = [[Cell]]

boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 20

-- Creates an empty board filled with Nothing (empty cells)
emptyBoard :: Board
emptyBoard = replicate boardHeight (replicate boardWidth Nothing)

-- Draws the board to the terminal
drawBoard :: Board -> IO ()
drawBoard board = do
    putStrLn "\ESC[2J"  -- Clear screen
    mapM_ drawRow board
  where
    drawRow row = putStrLn $ concatMap drawCell row
    drawCell Nothing  = ". "
    drawCell (Just c) = c : " "

-- Adds a tetromino's blocks to the board temporarily for display
updateBoardWithTetromino :: Board -> Tetromino -> Board
updateBoardWithTetromino board tetro =
    [ [ cellAt x y | x <- [0..boardWidth - 1] ]
    | y <- [0..boardHeight - 1] ]
  where
    blocks = tetrominoBlocks tetro
    cellAt x y =
      if (x, y) `elem` blocks
         then Just 'T'
         else board !! y !! x
