module Board
    ( Board
    , Cell
    , boardWidth
    , boardHeight
    , isValidPosition
    , mergeTetromino
    , emptyBoard
    , drawBoard
    , clearFullLines
    ) where

import Data.Maybe (isNothing)
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
    putStrLn "\ESC[H\ESC[2J"  -- Clear screen
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

-- Ensure the tetromino's blocks are inside the board bounds
isValidPosition :: Tetromino -> Board -> Bool
isValidPosition tetromino board = all inBounds coords && all cellEmpty coords
  where
    coords = tetrominoBlocks tetromino
    inBounds (x, y) = x >= 0 && x < boardWidth && y >= 0 && y < boardHeight
    cellEmpty (x, y) = isNothing(board !! y !! x)

-- Lock a Tetromino permanently into the board cells
mergeTetromino :: Tetromino -> Board -> Board
mergeTetromino tetromino board = foldr place board (tetrominoBlocks tetromino)
  where
    s       = shape tetromino
    place (x, y) rows = 
      let oldRow = rows !! y
          newRow = take x oldRow ++ [Just s] ++ drop (x + 1) oldRow
      in take y rows ++ [newRow] ++ drop (y + 1) rows

-- Remove every completely filled row and return the new board plus the number of cleared lines
clearFullLines :: Board -> (Board, Int)
clearFullLines board = (newRows ++ blanks, cleared)
  where
    (full, rest) = span (all (not . isNothing)) board
    remaining    = filter (any isNothing) board
    cleared      = boardHeight - length remaining
    newRows      = replicate cleared (replicate boardWidth Nothing)
    blanks       = remaining