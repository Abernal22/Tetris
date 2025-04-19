module Board
  ( Board
  , Cell
  , boardWidth
  , boardHeight
  , isValidPosition
  , mergeTetromino
  , emptyBoard
  , clearFullLines
  , boardToBlocks
  ) where

import Data.Maybe (isJust, isNothing)
import Data.List (partition)
import Tetromino (Tetromino(..), Shape(..), tetrominoBlocks)
import Graphics.Gloss (Color, makeColorI)

-- Types
type Cell = Maybe Shape
type Board = [[Cell]]

boardWidth, boardHeight :: Int
boardWidth  = 10
boardHeight = 20

-- Initialize an empty board
emptyBoard :: Board
emptyBoard = replicate boardHeight (replicate boardWidth Nothing)

-- Check if a tetromino is in a valid position
isValidPosition :: Tetromino -> Board -> Bool
isValidPosition tetromino board = all inBounds coords && all cellEmpty coords
  where
    coords    = map (\(x, y, _) -> (x, y)) (tetrominoBlocks tetromino)
    inBounds (x, y) = x >= 0 && x < boardWidth && y >= 0 && y < boardHeight
    cellEmpty (x, y) = isNothing (board !! y !! x)

-- Merge tetromino into the board
mergeTetromino :: Tetromino -> Board -> Board
mergeTetromino tetromino board = foldr place board coords
  where
    s       = shape tetromino
    coords  = map (\(x, y, _) -> (x, y)) (tetrominoBlocks tetromino)
    place (x, y) rows =
      let row     = rows !! y
          newRow  = take x row ++ [Just s] ++ drop (x + 1) row
      in take y rows ++ [newRow] ++ drop (y + 1) rows

-- Clear full lines and return the new board and lines cleared
clearFullLines :: Board -> (Board, Int)
clearFullLines bd =
  let (fullRows, restRows) = partition (all isJust) bd
      n                    = length fullRows
      emptyRow             = replicate boardWidth Nothing
      newBoard             = replicate n emptyRow ++ restRows
  in (newBoard, n)

-- Convert the board into drawable blocks
boardToBlocks :: Board -> [(Int, Int, Color)]
boardToBlocks board = 
  [ (x, y, shapeColor s)
  | (y, row) <- zip [0..] board
  , (x, cell) <- zip [0..] row
  , Just s <- [cell]
  ]

-- Shape to Color mapping
shapeColor :: Shape -> Color
shapeColor s = case s of
  I -> makeColorI 0 255 255 255   -- Cyan
  O -> makeColorI 255 255 0 255   -- Yellow
  T -> makeColorI 160 32 240 255  -- Purple
  S -> makeColorI 0 255 0 255     -- Green
  Z -> makeColorI 255 0 0 255     -- Red
  J -> makeColorI 0 0 255 255     -- Blue
  L -> makeColorI 255 165 0 255   -- Orange
