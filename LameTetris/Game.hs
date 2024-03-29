{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             ScopedTypeVariables,
             OverlappingInstances #-}

module LameTetris.Game where

import Prelude
import qualified Prelude as P
import System.Random
import Data.Maybe (catMaybes, isJust)
import Data.List (foldl', sort, findIndex)
import Control.Monad (when)
import Control.Monad.RWS.Strict
import Data.Array.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector

import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF

import LameTetris.Types
import qualified LameTetris.Types as LT
import LameTetris.Video
import LameTetris.Timer
import LameTetris.Utils

-- Random stuff
-- Add an instance to random so that I can make random BlockTypes
instance (Enum a, Bounded a) => Random a where
  randomR (mn, mx) g = let fstToEnum (a, g) = (toEnum a, g)
                       in fstToEnum $ randomR (fromEnum mn, fromEnum mx) g
  random = randomR (minBound :: a, maxBound :: a)

-- | Grab a random block
spawnBlock' :: IO Block
spawnBlock' = do btype <- liftIO $ randomRIO (minBound, maxBound)
                 return $ Block { position = (4, 0)
                                , guts = (getGuts btype)
                                }
-- | for use inside Game monad
spawnBlock :: Game Block                 
spawnBlock = liftIO spawnBlock'                   
  
-- | Empty board
startBoard :: Board
-- startBoard = fromListVector (R.Z :. 10 :. 20) $ replicate 200 Nothing
startBoard = fromListVector (R.Z :. 10 :. 20) $ replicate 200 Nothing

-- | Initial game state
initialGS :: Font -> IO GameState
initialGS fnt = do time <- initialTimer
                   linetxt <- renderTextSolid fnt "0" textColor
                   frst <- spawnBlock'
                   next <- spawnBlock'
                   return $ GameState { lineNum = 0
                                      , timer = time
                                      , currentPiece = frst
                                      , nextPiece = next { position = (12, 8) }
                                      , board = startBoard
                                      , linesText = linetxt
                                      , gameOver = False
                                      }

-- | Handle events, then draw the game, then do it again (unless you quit)
gameLoop :: Game ()
gameLoop = do
  event <- handlePoll
  over <- gets gameOver
  when (event /= Quit && not over) $
    do brd <- gets board
       drawBoard brd
       handleInterval
       waitUntilNextFrame
       gameLoop


-- | Checks if the interval has been reached, if so, move down!
handleInterval :: Game ()                            
handleInterval = do
  time <- gets timer
  now <- liftIO $ getTicks
  let lastDropDelta = now - (tsld time)
      interv = interval time
  when (lastDropDelta >= interv) $ do ifM currentPieceShouldBeSet -- condition
                                        setPieceAndStartAnew -- then
                                        (movePiece 0 1) -- else
                                      linum <- gets lineNum
                                      let newInt = calculateInterval linum
                                      setTimer $ time { interval = newInt
                                                      , tsld = now
                                                      }


-- | Polls and polls until it can't poll no more!
handlePoll :: Game Event
handlePoll = do
  event <- liftIO pollEvent
  case event of Quit -> return Quit
                NoEvent -> return NoEvent
                _ -> handleEvent event >> handlePoll


-- | In any event, this is what you do:
handleEvent :: Event -> Game ()
handleEvent (KeyUp (Keysym SDLK_LEFT _ _))  = movePiece (-1) 0
handleEvent (KeyUp (Keysym SDLK_RIGHT _ _)) = movePiece 1 0
handleEvent (KeyUp (Keysym SDLK_DOWN _ _))  = movePiece 0 1
handleEvent (KeyUp (Keysym SDLK_UP _ _)) = rotateCurrent

handleEvent _ = return ()


-- | Checks if two grids collide
collide :: Grid -> Grid -> Bool
collide g1 g2 = R.foldAllS (||) False $ R.zipWith (bothJust) g1 g2
  

-- | Takes a point (position current), a source (guts current),
  -- and a destination (board), and 
combineAt :: Point -> Grid -> Grid -> Grid
combineAt (x,y) src dest = computeVectorS $ traverse dest id insertCell
  where
    (R.Z :. w :. h) = extent src
    insertCell lookupDest spot@(R.Z :. destX :. destY) =
      if any id [ destX < x
                , destX >= x + w
                , destY < y
                , destY >= y + h
                ]
      then lookupDest spot -- No changes here, keep moving...
      else case src ! (R.Z :. destX -x :. destY - y) of
        Nothing -> lookupDest spot -- Nothing new, keep moving...
        occupiedCell -> occupiedCell -- There's something to add, so use that!


-- | Checks if there are any full rows on the board and removes them
removeFullRowsFromBoard :: Game ()
removeFullRowsFromBoard = do
  brd <- gets board
  let fullRows = scanFullRows brd
  when (not $ null fullRows) $ do addRowsToCount $ length fullRows                                  
                                  setBoard $ removeRows brd fullRows
    
-- | Takes a board and a list of y-indexed rows, then returns a board
  -- without those rows (padded with Nothings on top)
removeRows :: Board -> [Int] -> Board
removeRows brd rows = computeVectorS $ traverse brd id determineCell
  where
    numToDrop = length rows
    sh = extent brd
    determineCell lookupBrd spot@(R.Z :. x :. y)
      | y < numToDrop = Nothing
      | otherwise =
        case findIndex (== y) $ reverse $ sort (y:rows) of
             Nothing -> error "what is this I don't even... I JUST PUT THAT THERE"
                        -- this is for cases when the y index of the cell is also a row
                        -- that was just removed :(
             Just offset -> let offset' = if (y `elem` rows) then offset + 1 else offset
                                y' = properY (y - offset')
                            in lookupBrd (R.Z :. x :. y')
    properY y
      | y `elem` rows = properY (y - 1)
      | otherwise = y





-- | Sees if there are any full rows - they mean points!!
  -- [] means no rows were full
  -- [1, 3] means rows 1 and 3 were full!!!
scanFullRows :: Grid -> [Int]
scanFullRows grid = catMaybes . P.map checkFull $ rows
  where
    (R.Z :. w :. h) = extent grid
    getRow y = computeVectorS $ extract (R.Z :. 0 :. y) (R.Z :. 10 :. 1) grid
    range = [0 .. h -1]
    -- e.g. rows = [(0, Nothing), (1, Just J) ... ]
    rows = P.zip range $ P.map getRow range

    checkFull :: (Int, Grid) -> Maybe Int
    checkFull (ix, row) = do isFull <- foldAllP (&&) True $ R.map isJust row
                             case isFull of
                               False -> Nothing -- Ain't full, please disregard
                               True -> Just ix -- Full row here, remember the index!

-- | handles change in row count
addRowsToCount :: Int -> Game ()                               
addRowsToCount ct = do linum <- gets lineNum
                       linetxt <- gets linesText
                       fnt <- asks font
                       let newct = linum + (toEnum ct)
                       setLineNum newct
                       liftIO $ freeSurface linetxt
                       newlinetxt <- liftIO $ renderTextSolid fnt (show newct) textColor
                       setLinesText newlinetxt



-- | Checks if we should set the current piece into the board and move on                    
currentPieceShouldBeSet :: Game Bool
currentPieceShouldBeSet = fmap not $ checkCurrentPieceCanMove 0 1

-- | Checks if the current piece could be moved to a location
checkCurrentPieceCanMove :: Int -> Int -> Game Bool
checkCurrentPieceCanMove dx dy = do
  current <- gets currentPiece
  checkPieceCanMove current dx dy

-- | Checks if the a particular piece could be moved to the location
checkPieceCanMove :: Block -> Int -> Int -> Game Bool
checkPieceCanMove block dx dy = do
  brd <- gets board
  let (x,y) = position block
      currentSlice = guts block
      currentsh@(R.Z :. w :. h) = extent currentSlice
      -- y+1 gets us the 
      boardSlice = computeVectorS $ extract (R.Z :. x+dx :. y+dy) currentsh brd
  if (y + h) >= boardHeight || (x + dx) < 0 || (x + dx) > (boardWidth - w) -- board edge check
    then return False -- Bottom of the board, left side of board, right side
    else let doesntCollide = not $ collide currentSlice boardSlice -- board contents check
         in return doesntCollide


-- | Board bounds checked move: won'd do anything if it can't move
movePiece :: Int -> Int -> Game ()
movePiece dx dy = do
  canMove <- checkCurrentPieceCanMove dx dy
  when canMove $ movePiece' dx dy
  where
    movePiece' dx dy = do
      current <- gets currentPiece
      let (x,y) = position current
      setCurrentPiece $ current { position = ((x + dx), (y + dy)) }


-- | Takes the current piece, slaps it on the board, and starts everything
  -- all over again
setPieceAndStartAnew :: Game ()
setPieceAndStartAnew = do
  newNextBlock <- spawnBlock
  gs <- get
  let current = currentPiece gs
      nextp = (nextPiece gs) { position = (4, 1) }
      newBoard = combineAt (position current) (guts current) (board gs)
  put $ gs { currentPiece = nextp
           , nextPiece = newNextBlock { position = (12, 8) }
           , board = newBoard
           }
  removeFullRowsFromBoard
  canSetNew <- checkPieceCanMove nextp 0 0
  when (not canSetNew) $ setGameOver True
  


rotateCurrent :: Game ()
rotateCurrent = gets currentPiece >>= rotate >>= setCurrentPiece
 where
   rotate :: Block -> Game Block
   rotate block@(Block pos@(px,py) grid) = do
     let (R.Z :. oldw :. oldh) = extent grid
         w' = oldh -- height becomes width
         h' = oldw -- width becomes height
         newsh = (R.Z :. w' :. h')
         -- make sure that the px and py values fit the piece on the board!
         permissibleX x
           | x < 0 = 0
           | x >= (boardWidth - w') = boardWidth - w'
           | otherwise = x
         permissibleY y
           | y < 0 = 0
           | y >= (boardHeight - h') = boardHeight - h'
           | otherwise = y
         newpos@(newx, newy) = (permissibleX px, permissibleY py)
         -- shuffle pieces over to their new positions
         translateC (R.Z :. x :. y) = (R.Z :. (oldw - y - 1) :. x)
         newgrid = computeVectorS $ backpermute newsh translateC grid
         newblock = Block { position = newpos, guts = newgrid }
     canMove <- checkPieceCanMove newblock 0 0
     return $ if canMove
              then newblock
              else block -- can't move, keep same
