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
import Data.List  (partition)
import Tetromino  (Tetromino(..), Shape(..), tetrominoBlocks)
import Brillo.Data.Color (Color, cyan, yellow, magenta, green, red, blue, white)

-- | A cell is either empty or holds a Shape
type Cell  = Maybe Shape
-- | The board is a grid of cells (rows of width boardWidth)
type Board = [[Cell]]

boardWidth, boardHeight :: Int
boardWidth  = 10
boardHeight = 20

-- | An empty board: all cells Nothing
emptyBoard :: Board
emptyBoard = replicate boardHeight (replicate boardWidth Nothing)

-- | Check that every block of the Tetromino is in bounds and on an empty cell
isValidPosition :: Tetromino -> Board -> Bool
isValidPosition tet board = all inBounds coords && all cellEmpty coords
  where
    coords    = [ (x,y)        | (x,y,_) <- tetrominoBlocks tet ]
    inBounds (x,y)   = x >= 0 && x < boardWidth && y >= 0 && y < boardHeight
    cellEmpty (x,y)  = isNothing (board !! y !! x)

-- | Merge a landed Tetromino into the board
mergeTetromino :: Tetromino -> Board -> Board
mergeTetromino tet board = foldr place board coords
  where
    s      = shape tet
    coords = [ (x,y) | (x,y,_) <- tetrominoBlocks tet ]
    place (x,y) rows =
      let row    = rows !! y
          newRow = take x row ++ [Just s] ++ drop (x+1) row
      in take y rows ++ [newRow] ++ drop (y+1) rows

-- | Clear full lines, returning (newBoard, numberCleared)
clearFullLines :: Board -> (Board, Int)
clearFullLines bd =
  let (full, rest) = partition (all isJust) bd
      n            = length full
      emptyRow     = replicate boardWidth Nothing
  in (replicate n emptyRow ++ rest, n)

-- | Convert board to drawable blocks (x,y,Color)
boardToBlocks :: Board -> [(Int, Int, Color)]
boardToBlocks bd =
  [ (x, y, shapeColor s)
  | (y, row) <- zip [0..] bd
  , (x, cell) <- zip [0..] row
  , Just s <- [cell]
  ]

-- | Map a Shape to its Brillo Color
shapeColor :: Shape -> Color
shapeColor I = cyan
shapeColor O = yellow
shapeColor T = magenta
shapeColor S = green
shapeColor Z = red
shapeColor J = blue
shapeColor L = white
