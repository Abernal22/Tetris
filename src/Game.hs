module Game (runGame) where

import Control.Concurrent (threadDelay)
import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering), hReady)
import Board (Board, emptyBoard, drawBoard, updateBoardWithTetromino)
import Tetromino (Tetromino(..), initialPosition, tetrominoBlocks, randomShape)

import System.IO (stdin, hReady, hSetBuffering, hSetEcho, BufferMode(NoBuffering))

data GameState = GameState
  { board :: Board
  , tetromino :: Tetromino
  }

runGame :: IO ()
runGame = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  shape <- randomShape
  let initialState = GameState
        { board = emptyBoard
        , tetromino = Tetromino shape initialPosition
        }
  gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state = do
  putStrLn "\ESC[2J" -- clear screen
  let GameState b t = state
      b' = updateBoardWithTetromino b t
  drawBoard b'
  
  hasInput <- hReady stdin
  dir <- if hasInput then getChar else return ' '

  let tMoved = case dir of
                 'a' -> moveLeft t
                 'd' -> moveRight t
                 _   -> t

  let t' = moveDown (if isValidPosition tMoved then tMoved else t)

  threadDelay 500000  -- half a second

  if isValidPosition t'
    then gameLoop state { tetromino = t' }
    else do
      -- Lock current piece into the board
      let lockedBoard = updateBoardWithTetromino b t
      -- Spawn a new piece
      newShape <- randomShape
      let newTetromino = Tetromino newShape initialPosition
      if isValidPosition newTetromino
        then gameLoop GameState { board = lockedBoard, tetromino = newTetromino }
        else putStrLn "Game Over!"


readInput :: IO (Maybe Char)
readInput = do
  ready <- hReady stdin
  if ready then Just <$> getChar else return Nothing

moveDown :: Tetromino -> Tetromino
moveDown (Tetromino s (x, y)) = Tetromino s (x, y + 1)

moveLeft :: Tetromino -> Tetromino
moveLeft (Tetromino s (x, y)) = Tetromino s (x - 1, y)

moveRight :: Tetromino -> Tetromino
moveRight (Tetromino s (x, y)) = Tetromino s (x + 1, y)

-- Ensure the tetromino's blocks are inside the board bounds
isValidPosition :: Tetromino -> Bool
isValidPosition tetromino = all inBounds (tetrominoBlocks tetromino)
  where
    inBounds (x, y) = x >= 0 && x < 10 && y >= 0 && y < 20