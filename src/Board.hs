module Board
    ( Board
    , Cell
    , emptyBoard
    , drawBoard
    , updateBoardWithTetromino
    ) where

import Tetromino (Tetromino(..), Shape(..), tetrominoBlocks)

type Cell = Maybe Shape
type Board = [[Cell]]

boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 20

-- Creates an empty board filled with Nothing (empty cells)
emptyBoard :: Board
emptyBoard = replicate boardHeight (replicate boardWidth Nothing)

-- Draws the board to the terminal with borders
drawBoard :: Board -> IO ()
drawBoard board = do
    putStrLn "\ESC[2J"  -- Clear screen
    putStrLn $ "┌" ++ replicate (boardWidth * 2) '─' ++ "┐"
    mapM_ drawRow board
    putStrLn $ "└" ++ replicate (boardWidth * 2) '─' ++ "┘"
  where
    drawRow row = putStrLn $ "│" ++ concatMap drawCell row ++ "│"
    drawCell Nothing       = ". "
    drawCell (Just shape)  = colorize shape

-- Maps shapes to colored block strings
colorize :: Shape -> String
colorize shape = colorCode shape ++ "██" ++ reset
  where
    reset = "\ESC[0m"
    colorCode s = case s of
        I -> "\ESC[96m"  -- Bright Cyan
        O -> "\ESC[93m"  -- Bright Yellow
        T -> "\ESC[95m"  -- Bright Magenta
        S -> "\ESC[92m"  -- Bright Green
        Z -> "\ESC[91m"  -- Bright Red
        J -> "\ESC[94m"  -- Bright Blue
        L -> "\ESC[33m"  -- Orange-like Yellow

-- Adds a tetromino's blocks to the board temporarily for display
updateBoardWithTetromino :: Board -> Tetromino -> Board
updateBoardWithTetromino board tetro@(Tetromino s _) =
    [ [ cellAt x y | x <- [0..boardWidth - 1] ]
    | y <- [0..boardHeight - 1] ]
  where
    blocks = tetrominoBlocks tetro
    cellAt x y =
      if (x, y) `elem` blocks
         then Just s
         else board !! y !! x
