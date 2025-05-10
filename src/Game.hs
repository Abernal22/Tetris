module Game (gameMain) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)

import Board
  ( Board
  , emptyBoard
  , isValidPosition
  , mergeTetromino
  , clearFullLines
  , boardToBlocks
  )

import Tetromino
  ( Tetromino(..)
  , randomShape
  , initialPosition
  , tetrominoBlocks
  , moveBy
  , rotateRight
  , rotateLeft
  )

import Text.Printf (printf)

-- | One block is 20×20 pixels
blockSize :: Float
blockSize = 20

-- | Left panel width in blocks (for Hold/Next & status)
panelWidthBlocks :: Int
panelWidthBlocks = 6

panelOffset :: Float
panelOffset = fromIntegral panelWidthBlocks * blockSize

-- | Board dimensions (in blocks)
boardWidth, boardHeight :: Int
boardWidth  = 10
boardHeight = 20

-- | Window size in pixels (board + left panel)
windowWidth, windowHeight :: Float
windowWidth  = fromIntegral boardWidth  * blockSize + panelOffset
windowHeight = fromIntegral boardHeight * blockSize

-- | Game state, including hold/next logic
data GameState = GameState
  { board      :: Board
  , current    :: Tetromino
  , nextT      :: Tetromino
  , holdPiece  :: Maybe Tetromino
  , usedHold   :: Bool       -- only one hold until next lock
  , score      :: Int
  , level      :: Int
  , linesTotal :: Int
  , timeAccum  :: Float
  , gameOver   :: Bool
  }

-- | Initialize two random pieces and start the game
gameMain :: IO ()
gameMain = do
  s1 <- randomShape
  s2 <- randomShape
  let t1 = Tetromino s1 initialPosition 0
      t2 = Tetromino s2 initialPosition 0
      initSt = GameState
        { board      = emptyBoard
        , current    = t1
        , nextT      = t2
        , holdPiece  = Nothing
        , usedHold   = False
        , score      = 0
        , level      = 0
        , linesTotal = 0
        , timeAccum  = 0
        , gameOver   = False
        }
  playIO
    (InWindow "TETRIS" (round windowWidth, round windowHeight) (100,100))
    black
    60
    initSt
    drawGame
    handleEvent
    updateGame

-- | Draw every frame
drawGame :: GameState -> IO Picture
drawGame st =
  if gameOver st
    then return $ Pictures
      [ Color red
          $ Translate (-80) 0
          $ Scale 0.3 0.3
          $ Text "Game Over"
      , Color white
          $ Translate (-85) (-30)
          $ Scale 0.2 0.2
          $ Text ("Final Score: " ++ show (score st))
      ]
    else return $ Pictures $
      -- 1) Board border
      [ Color white
          $ Translate boardCenterX 0
          $ rectangleWire
              (fromIntegral boardWidth  * blockSize)
              (fromIntegral boardHeight * blockSize)
      ]
      -- 2) Locked cells + current piece
      ++ map drawBlock (boardToBlocks (board st))
      ++ map drawBlock (tetrominoBlocks (current st))
      -- 3) Hold box + piece + label
      ++ [ Color white
             $ Translate panelCenterX holdCenterY
             $ rectangleWire (4*blockSize) (4*blockSize)
         , drawBoxedTetromino (holdPiece st) panelCenterX holdCenterY
         , Translate holdLabelX holdLabelY
             $ Scale 0.08 0.08
             $ Color white
             $ Text "Hold"
         ]
      -- 4) Next box + next piece + label
      ++ [ Color white
             $ Translate panelCenterX previewCenterY
             $ rectangleWire (4*blockSize) (4*blockSize)
         , drawBoxedTetromino (Just (nextT st)) panelCenterX previewCenterY
         , Translate previewLabelX previewLabelY
             $ Scale 0.08 0.08
             $ Color white
             $ Text "Next"
         ]
      -- 5) Status text stacked vertically
      ++ [ Translate statusX statusLine1Y
             $ Scale 0.08 0.08
             $ Color white
             $ Text (printf "Score: %d" (score st))
         , Translate statusX statusLine2Y
             $ Scale 0.08 0.08
             $ Color white
             $ Text (printf "Level: %d" (level st))
         , Translate statusX statusLine3Y
             $ Scale 0.08 0.08
             $ Color white
             $ Text (printf "Speed: %.2fs" (dropInterval (level st)))
         ]
  where
    -- Draw a single block given board coords
    drawBlock (x, y, col) =
      Translate
        ( fromIntegral x * blockSize
          - windowWidth/2
          + blockSize/2
          + panelOffset
        )
        (-( fromIntegral y * blockSize
            - windowHeight/2
            + blockSize/2
          ))
      $ Color col
      $ rectangleSolid (blockSize - 2) (blockSize - 2)

    -- Draw a Maybe Tetromino inside a 4×4 box at (cx,cy)
    drawBoxedTetromino :: Maybe Tetromino -> Float -> Float -> Picture
    drawBoxedTetromino Nothing _ _ = Blank
    drawBoxedTetromino (Just t) cx cy =
      let t0 = t { position = (0,0) }
      in Pictures
         [ Translate (cx + fromIntegral x*blockSize)
                     (cy + fromIntegral y*blockSize)
             $ Color col
             $ rectangleSolid (blockSize - 2) (blockSize - 2)
         | (x,y,col) <- tetrominoBlocks t0
         ]

    -- vertical gap between Hold and Next boxes
    boxSpacing :: Float
    boxSpacing = 20

    -- Panel & board centers
    panelCenterX   = -windowWidth/2 + panelOffset/2
    holdCenterY    =  windowHeight/2    - 2*blockSize - 10
    previewCenterY = holdCenterY       - 4*blockSize - boxSpacing
    boardCenterX   = panelOffset
                     - windowWidth/2
                     + (fromIntegral boardWidth * blockSize)/2

    -- horizontal centering for labels and status (box is 80px wide)
    labelOffsetX    = blockSize/2
    holdLabelX      = panelCenterX - labelOffsetX
    previewLabelX   = panelCenterX - labelOffsetX
    statusX         = panelCenterX - labelOffsetX

    -- Y positions for labels
    holdLabelY      = holdCenterY    - (4*blockSize/2) - 10
    previewLabelY   = previewCenterY - (4*blockSize/2) - 10

    -- Status text stacking under preview label
    statusStartY    = previewLabelY - 20
    statusSpacing   = 15
    statusLine1Y    = statusStartY
    statusLine2Y    = statusStartY - statusSpacing
    statusLine3Y    = statusStartY - 2 * statusSpacing

-- | Handle key events: move, rotate, hold, hard-drop
handleEvent :: Event -> GameState -> IO GameState
handleEvent ev st
  | gameOver st = return st
  | otherwise   = case ev of
      EventKey (Char 'a') Down _ _ -> return $ tryMove (-1,0) st
      EventKey (Char 'd') Down _ _ -> return $ tryMove ( 1,0) st
      EventKey (Char 's') Down _ _ -> return $ tryMove ( 0,1) st
      EventKey (Char 'w') Down _ _ -> return $ tryRotate rotateRight st
      EventKey (Char 'q') Down _ _ -> return $ tryRotate rotateLeft  st
      EventKey (Char 'c') Down _ _ -> holdCurrent st
      EventKey (SpecialKey KeySpace) Down _ _ -> return $ hardDrop st
      _ -> return st

-- | Try to move current tetromino
tryMove :: (Int,Int) -> GameState -> GameState
tryMove d st =
  let m = moveBy d (current st)
  in if isValidPosition m (board st)
       then st { current = m }
       else st

-- | Try rotation with wall-kicks
tryRotate :: (Tetromino -> Tetromino) -> GameState -> GameState
tryRotate f st =
  let raw   = f (current st)
      kicks = [ moveBy off raw | off <- [(0,0),(1,0),(-1,0),(0,-1)] ]
      vs    = filter (`isValidPosition` board st) kicks
  in case vs of (t':_) -> st { current = t' }
                []     -> st

-- | Hold logic on 'c'
holdCurrent :: GameState -> IO GameState
holdCurrent st
  | usedHold st = return st
  | otherwise   = case holdPiece st of
      Nothing -> do
        s <- randomShape
        let newHold = current st
            newCurr = (nextT st){ position = initialPosition, orientation = 0 }
            newNext = Tetromino s initialPosition 0
        return st
          { holdPiece = Just newHold
          , current   = newCurr
          , nextT     = newNext
          , usedHold  = True
          }
      Just h -> do
        let newCurr = h { position = initialPosition, orientation = 0 }
        return st
          { holdPiece = Just (current st)
          , current   = newCurr
          , usedHold  = True
          }

-- | Gravity tick; lock & spawn when necessary
updateGame :: Float -> GameState -> IO GameState
updateGame dt st
  | gameOver st = return st
  | timeAccum st + dt >= dropInterval (level st) = do
      let st'   = st { timeAccum = 0 }
          moved = tryMove (0,1) st'
      if position (current moved) == position (current st)
        then lockAndNext st'
        else return moved
  | otherwise = return st { timeAccum = timeAccum st + dt }

-- | Lock piece, clear lines, update metrics, spawn next
lockAndNext :: GameState -> IO GameState
lockAndNext st = do
  let merged   = mergeTetromino (current st) (board st)
      (bd',n)  = clearFullLines merged
      tlines   = linesTotal st + n
      lvl      = tlines `div` 10
      sc'      = score st + scoreForLines n (level st)
      nextCur  = (nextT st){ position = initialPosition, orientation = 0 }
  s <- randomShape
  let newNext = Tetromino s initialPosition 0
  if isValidPosition nextCur bd'
    then return st
      { board      = bd'
      , current    = nextCur
      , nextT      = newNext
      , usedHold   = False
      , score      = sc'
      , linesTotal = tlines
      , level      = lvl
      , timeAccum  = 0
      }
    else return st { gameOver = True }

-- | Scoring per lines cleared
scoreForLines :: Int -> Int -> Int
scoreForLines n lvl = case n of
  1 -> 100 * (lvl + 1)
  2 -> 300 * (lvl + 1)
  3 -> 500 * (lvl + 1)
  4 -> 800 * (lvl + 1)
  _ -> 0

-- | Hard drop until collision
hardDrop :: GameState -> GameState
hardDrop st =
  let go t
        | isValidPosition (moveBy (0,1) t) (board st) = go (moveBy (0,1) t)
        | otherwise                                   = t
  in st { current = go (current st) }

-- | Seconds between automatic drops (decreases with level)
dropInterval :: Int -> Float
dropInterval lvl = max 0.05 (0.4 - fromIntegral lvl * 0.03)
