module Game (runGame) where

<<<<<<< HEAD
import Control.Concurrent (threadDelay)
import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering), hReady)
import Board (Board, emptyBoard, drawBoard, updateBoardWithTetromino)
import Tetromino (Tetromino(..), initialPosition, tetrominoBlocks, randomShape, Rotation(..), rotateTetromino)
=======
import Board
  ( Board
  , emptyBoard
  , drawBoard
  , isValidPosition
  , mergeTetromino
  , clearFullLines
  , boardHeight
  )

import Tetromino
  ( Tetromino(..)
  , moveBy
  , rotateRight
  , rotateLeft
  , randomShape
  , initialPosition
  )

import System.IO
  ( stdin
  , hReady
  , hSetBuffering
  , hSetEcho
  , BufferMode(NoBuffering)
  )

import Control.Concurrent (threadDelay)
>>>>>>> main.josh

data GameState = GameState
  { board :: Board
  , current :: Tetromino
  , score :: Int
  , level :: Int
  , linesTotal :: Int
  }

runGame :: IO ()
runGame = do
  hSetBuffering stdin NoBuffering
  hSetEcho      stdin False
  shape <- randomShape
<<<<<<< HEAD
  let initialState = GameState
        { board = emptyBoard
        , tetromino = Tetromino shape initialPosition R0
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
                 'w' -> rotateTetromino t
                 _   -> t
=======
  let initTetro = Tetromino shape initialPosition 0
      initState = GameState emptyBoard initTetro 0 0 0
  gameLoop initState

gameLoop :: GameState -> IO ()
gameLoop state = do
  -- render
  let previewBoard = mergeTetromino (current state) (board state)
  drawBoard previewBoard
  putStrLn $ "Score: " ++ show (score state) ++ "  Level: " ++ show (level state)
  -- user input
  input <- getInputChar
  let moved = handleInput input state
  -- gravity drop based on level
  let interval = max 10000 (600000 - level moved * 50000) -- micro secondds
  threadDelay interval

  let afterGrav = tryMoveCurrent (0, 1) moved
>>>>>>> main.josh

  -- Deal with any locks, clears, or spawn / check game over
  newState <- if unchanged afterGrav moved
                then lockAndSpawn moved
                else return afterGrav
  gameLoop newState
 where
  unchanged state1 state2 = position (current state1) == position (current state2)

-- Input handling
handleInput :: Maybe Char -> GameState -> GameState
handleInput Nothing  = id
handleInput (Just c) = case c of
  'a' -> tryMoveCurrent (-1, 0)
  'd' -> tryMoveCurrent (1, 0)
  's' -> tryMoveCurrent (0, 1)
  'w' -> tryRotateCurrent rotateRight
  'q' -> tryRotateCurrent rotateLeft
  _   -> id

<<<<<<< HEAD
  if isValidPosition t'
    then gameLoop state { tetromino = t' }
    else do
      let lockedBoard = updateBoardWithTetromino b t
      newShape <- randomShape
      let newTetromino = Tetromino newShape initialPosition R0
      if isValidPosition newTetromino
        then gameLoop GameState { board = lockedBoard, tetromino = newTetromino }
        else putStrLn "Game Over!"

moveDown :: Tetromino -> Tetromino
moveDown (Tetromino s (x, y) r) = Tetromino s (x, y + 1) r

moveLeft :: Tetromino -> Tetromino
moveLeft (Tetromino s (x, y) r) = Tetromino s (x - 1, y) r

moveRight :: Tetromino -> Tetromino
moveRight (Tetromino s (x, y) r) = Tetromino s (x + 1, y) r

isValidPosition :: Tetromino -> Bool
isValidPosition tetromino = all inBounds (tetrominoBlocks tetromino)
  where
    inBounds (x, y) = x >= 0 && x < 10 && y >= 0 && y < 20
=======
getInputChar :: IO (Maybe Char)
getInputChar = do
  ready <- hReady stdin
  if ready then Just <$> getChar else return Nothing

-- Piece Manipulation Piece
tryMoveCurrent :: (Int, Int) -> GameState -> GameState
tryMoveCurrent delta state
  | isValidPosition moved (board state) = state { current = moved }
  | otherwise                           = state
 where
  moved = moveBy delta (current state)

tryRotateCurrent :: (Tetromino -> Tetromino) -> GameState -> GameState
tryRotateCurrent rotate state = case firstValid rotatedAttempts of
    Just ok -> state { current = ok }
    Nothing -> state
 where
  raw     = rotate (current state)
  offsets = [(0, 0), (1, 0), (-1, 0), (0, -1)]
  rotatedAttempts = [ moveBy offs raw | offs <- offsets ]
  firstValid = foldr (\t acc -> if isValidPosition t (board state) then Just t else acc) Nothing

-- Locking, line clearing and spawning
lockAndSpawn :: GameState -> IO GameState
lockAndSpawn state = do
  let lockedBoard                    = mergeTetromino (current state) (board state)
      (clearedBoard, linesCleared)   = clearFullLines lockedBoard
      totalLines'                    = linesTotal state + linesCleared
      level'                         = totalLines' `div` 10
      score'                         = score state + scoreForLines linesCleared (level state)
  shape <- randomShape                      
  let spawn = Tetromino shape initialPosition 0
  if isValidPosition spawn clearedBoard
    then return state { board = clearedBoard
                  , current = spawn
                  , linesTotal = totalLines'
                  , level = level'
                  , score = score' }
    else do
      drawBoard clearedBoard
      putStrLn "Game Over!"
      putStrLn $ "Final score: " ++ show score'
      ioError (userError "Game Over")

scoreForLines :: Int -> Int -> Int
scoreForLines num level = case num of
  1 -> 100 * (level + 1)
  2 -> 300 * (level + 1)
  3 -> 500 * (level + 1)
  4 -> 800 * (level + 1)
  _ -> 0
>>>>>>> main.josh
