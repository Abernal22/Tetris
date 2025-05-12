module Game (gameMain) where

import Brillo.Data.Display     (Display(InWindow))
import Brillo.Data.Picture     (Picture, color, translate, scale, text, rectangleSolid, rectangleWire, pictures)
import Brillo.Data.Color       (white, black, red)
import Brillo.Interface.IO.Game (playIO, Event(..), Key(..), KeyState(..), SpecialKey(..))

import Board   (Board, emptyBoard, isValidPosition, mergeTetromino, clearFullLines, boardToBlocks)
import Tetromino (Tetromino(..), tetrominoBlocks, Shape, initialPosition, randomShape, moveBy, rotateRight, rotateLeft)

import Text.Printf             (printf)

-- | One block is 20×20 pixels
blockSize :: Float
blockSize = 20

-- | Side panel width (in blocks)
panelWidthBlocks :: Int
panelWidthBlocks = 6
panelOffset :: Float
panelOffset = fromIntegral panelWidthBlocks * blockSize

-- | Board dimensions (in blocks)
boardWidth, boardHeight :: Int
boardWidth  = 10
boardHeight = 20

-- | Window size in pixels (board + panel)
windowWidth, windowHeight :: Float
windowWidth  = fromIntegral boardWidth  * blockSize + panelOffset
windowHeight = fromIntegral boardHeight * blockSize

-- | Complete game state, including next, hold, pause
data GameState = GameState
  { board      :: Board
  , current    :: Tetromino
  , next       :: Tetromino
  , hold       :: Maybe Tetromino
  , holdUsed   :: Bool       -- prevent multiple holds per drop
  , paused     :: Bool
  , score      :: Int
  , level      :: Int
  , linesTotal :: Int
  , timeAccum  :: Float
  , gameOver   :: Bool
  }

-- | Entry point
gameMain :: IO ()
gameMain = do
  s1 <- randomShape
  s2 <- randomShape
  let t1 = Tetromino s1 initialPosition 0
      t2 = Tetromino s2 initialPosition 0
      initState = GameState emptyBoard t1 t2 Nothing False False 0 0 0 0 False
  playIO
    (InWindow "TETRIS" (round windowWidth, round windowHeight) (100,100))
    black
    60
    initState
    drawGame
    handleEvent
    updateGame

-- | Render game state
drawGame :: GameState -> IO Picture
drawGame st =
  if gameOver st then
    return $ pictures
      [ color red
          $ translate (-80) 0
          $ scale 0.3 0.3
          $ text "Game Over"
      , color white
          $ translate (-85) (-30)
          $ scale 0.2 0.2
          $ text ("Final Score: " ++ show (score st))
      ]
  else
    let
      -- Panel positions
      panelX  = -windowWidth/2 + panelOffset/2
      holdY   =  windowHeight/2 - panelOffset/2
      nextY   =  holdY - panelOffset - 20

      -- Wireframes
      holdBox = color white $ translate panelX holdY $ rectangleWire panelOffset panelOffset
      nextBox = color white $ translate panelX nextY $ rectangleWire panelOffset panelOffset
      boardLeft = -windowWidth/2 + panelOffset
      boardW    = fromIntegral boardWidth  * blockSize
      boardH    = fromIntegral boardHeight * blockSize
      boardBox  = color white
                $ translate (boardLeft + boardW/2) 0
                $ rectangleWire boardW boardH

      -- Labels
      holdLabel  = color white $ translate panelX (holdY + panelOffset/2 + 10) $ scale 0.1 0.1 $ text "Hold"
      nextLabel  = color white $ translate panelX (nextY + panelOffset/2 + 10) $ scale 0.1 0.1 $ text "Next"

      -- Status (stacked)
      statusX    = -windowWidth/2 + 10
      statusY1   = -windowHeight/2 + 20
      statusY2   = statusY1 + 20
      scoreText  = color white $ translate statusX statusY1 $ scale 0.08 0.08 $ text ("Score: " ++ show (score st))
      levelText  = color white $ translate statusX statusY2 $ scale 0.08 0.08 $ text ("Level: " ++ show (level st))

      -- Draw main and preview blocks
      drawMain (x,y,col) = translate
          (fromIntegral x * blockSize + boardLeft + blockSize/2)
          (-(fromIntegral y * blockSize - windowHeight/2 + blockSize/2))
        $ color col
        $ rectangleSolid (blockSize - 2) (blockSize - 2)
      mainBlocks = tetrominoBlocks (current st) ++ boardToBlocks (board st)

      -- Preview helper: position blocks around box center
      drawPreview tet (x,y,col) = translate
          (panelX + fromIntegral x * blockSize)
          (yOffset + fromIntegral (-y) * blockSize)
        $ color col
        $ rectangleSolid (blockSize - 2) (blockSize - 2)
        where yOffset = nextY - panelOffset/4
      previewBlocks = tetrominoBlocks (next st){ position=(0,0) }
      holdBlocks    = maybe [] (\t -> tetrominoBlocks t{ position=(0,0) }) (hold st)

    in return $ pictures
         ( [ holdBox, nextBox, boardBox
           , holdLabel, nextLabel, scoreText, levelText
           ]
         ++ map drawMain mainBlocks
         ++ map (drawPreview (next st)) previewBlocks
         ++ map (drawPreview (next st)) holdBlocks
         )

-- | Handle key events
handleEvent :: Event -> GameState -> IO GameState
handleEvent ev st
  | gameOver st = return st
  | otherwise = case ev of
      EventKey (Char 'a') Down _ _ -> return $ tryMove (-1,0) st
      EventKey (Char 'd') Down _ _ -> return $ tryMove (1,0) st
      EventKey (Char 's') Down _ _ -> return $ tryMove (0,1) st
      EventKey (Char 'w') Down _ _ -> return $ tryRotate rotateRight st
      EventKey (Char 'q') Down _ _ -> return $ tryRotate rotateLeft st
      EventKey (Char 'c') Down _ _ -> holdSwap st
      EventKey (SpecialKey KeyEsc) Down _ _ -> return st { paused = not (paused st) }
      EventKey (SpecialKey KeySpace) Down _ _ -> return $ hardDrop st
      _ -> return st

-- | Apply move in GameState
tryMove :: (Int,Int) -> GameState -> GameState
tryMove d st
  | isValidPosition moved (board st) = st{ current = moved }
  | otherwise                        = st
  where moved = moveBy d (current st)

-- | Rotate with kicks
tryRotate :: (Tetromino->Tetromino) -> GameState -> GameState
tryRotate rot st = case firstValid of
    Just t  -> st{ current = t }
    Nothing -> st
  where raw        = rot (current st)
        kicks      = [(0,0),(1,0),(-1,0),(0,-1)]
        candidates = [ moveBy off raw | off <- kicks ]
        firstValid = foldr (\t acc -> if isValidPosition t (board st) then Just t else acc) Nothing candidates

-- | Hold/pause swap logic
holdSwap :: GameState -> IO GameState
holdSwap st
  | holdUsed st = return st
  | otherwise   = do
    let cur = current st
        mHold = hold st
    case mHold of
      Nothing -> do
        s <- randomShape
        let newHold = cur{ position=initialPosition, orientation=0 }
            newCur  = next st
            newNext = Tetromino s initialPosition 0
        return st{ current=newCur, next=newNext, hold=Just newHold, holdUsed=True }
      Just h ->
        let newHold = cur{ position=initialPosition, orientation=0 }
            newCur  = h
        in return st{ current=newCur, hold=Just newHold, holdUsed=True }

-- | Time-based updates
updateGame :: Float -> GameState -> IO GameState
updateGame _ st
  | paused st  = return st
  | gameOver st = return st
updateGame dt st
  | timeAccum st + dt >= interval = do
      let moved = tryMove (0,1) st
      if position (current moved) == position (current st)
        then lockAndSpawn st{ timeAccum=0 }
        else return moved{ timeAccum=0 }
  | otherwise = return st{ timeAccum = timeAccum st + dt }
  where interval = max 0.05 (0.4 - fromIntegral (level st) * 0.03)

-- | Lock tetromino & spawn next/reset hold
lockAndSpawn :: GameState -> IO GameState
lockAndSpawn st = do
  let merged          = mergeTetromino (current st) (board st)
      (newBd,n)       = clearFullLines merged
      total'          = linesTotal st + n
      lvl'            = total' `div` 10
      scr'            = score st + scoreForLines n (level st)
  s <- randomShape
  let newCur  = next st
      newNext = Tetromino s initialPosition 0
  if isValidPosition newCur newBd
    then return st{ board=newBd, current=newCur, next=newNext, holdUsed=False
                  , score=scr', level=lvl', linesTotal=total', timeAccum=0 }
    else return st{ gameOver=True }

-- | Scoring
type LinesCleared = Int
scoreForLines :: LinesCleared -> Int -> Int
scoreForLines n lvl = case n of
  1 -> 100 * (lvl + 1)
  2 -> 300 * (lvl + 1)
  3 -> 500 * (lvl + 1)
  4 -> 800 * (lvl + 1)
  _ -> 0

-- | Hard drop
hardDrop :: GameState -> GameState
hardDrop st =
  let go t
        | isValidPosition (moveBy (0,1) t) (board st) = go (moveBy (0,1) t)
        | otherwise                                   = t
  in st { current = go (current st) }

-- | Drop interval
dropInterval :: Int -> Float
dropInterval lvl = max 0.05 (0.4 - fromIntegral lvl * 0.03)