module Game (runGame) where

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
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)

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
  let initTetro = Tetromino shape initialPosition 0
      initState = GameState emptyBoard initTetro 0 0 0
  start <- getCurrentTime
  gameLoop initState start

-- Main game loop with smooth input and timed gravity
gameLoop :: GameState -> UTCTime -> IO ()
gameLoop state lastDropTime = do
  now <- getCurrentTime
  let interval = fromIntegral (max 10000 (600000 - level state * 50000)) / 1000000 -- in seconds
      timePassed = realToFrac (diffUTCTime now lastDropTime)

  -- Check user input
  input <- getInputChar
  let movedState = handleInput input state

  -- Check if gravity should apply
  if timePassed >= interval
    then do
      let afterGrav = tryMoveCurrent (0, 1) movedState
      newState <- if unchanged afterGrav movedState
                    then lockAndSpawn movedState
                    else return afterGrav
      drawFrame newState
      now' <- getCurrentTime
      gameLoop newState now'
    else do
      drawFrame movedState
      threadDelay 20000 -- 20 ms for smoothness
      gameLoop movedState lastDropTime
 where
  unchanged state1 state2 = position (current state1) == position (current state2)

drawFrame :: GameState -> IO ()
drawFrame state = do
  let previewBoard = mergeTetromino (current state) (board state)
  drawBoard previewBoard
  putStrLn $ "Score: " ++ show (score state) ++ "  Level: " ++ show (level state)

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

getInputChar :: IO (Maybe Char)
getInputChar = do
  ready <- hReady stdin
  if ready then Just <$> getChar else return Nothing

-- Piece manipulation
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
