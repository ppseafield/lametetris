{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             ScopedTypeVariables,
             OverlappingInstances #-}

module LameTetris.Game where

import Prelude
import qualified Prelude as P
import System.Random
import Control.Monad (when)
import Control.Monad.RWS.Strict
import Data.Array.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import Debug.Trace (trace)

import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL

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
startBoard = fromListVector (R.Z :. 10 :. 20) $ (replicate 190 Nothing) P.++ (replicate 10 (Just L))

-- | Initial game state
initialGS :: IO GameState
initialGS = do time <- initialTimer
               frst <- spawnBlock'
               next <- spawnBlock'
               return $ GameState { lineNum = 0
                                  , timer = time
                                  , currentPiece = frst
                                  , nextPiece = next { position = (12, 8) }
                                  , board = startBoard
                                  }

-- | Handle events, then draw the game, then do it again (unless you quit)
gameLoop :: Game ()
gameLoop = do
  event <- handlePoll
  when (event /= Quit) $ do drawBoard startBoard
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
  when (lastDropDelta >= interv) $ do return ()
                                      ifM checkPieceShouldBeSet
                                        setPieceAndStartAnew
                                        (movePiece 0 1)
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



-- | Moves a piece in some direction
  -- movePiece (-1) 0 moves the current piece left
  -- moviePiece 0 1 movies it down
movePiece :: Int -> Int -> Game()
movePiece dx dy = do gs <- get
                     let current = currentPiece gs
                         (x, y) = position current
                         (R.Z :. w :. h) = extent $ guts current
                         targetx = x + dx -- new x is currentx + change in x
                         targety = y + dy -- new y is currenty + change in y
                         -- Make sure that we don't go off the board!
                         -- TODO Make sure that we don't walk into stuff on the board!
                         x' = if targetx > (boardWidth - w) || targetx < 0 then x else targetx
                         y' = if targety > (boardHeight - h) then y else targety
                     put $ gs { currentPiece = current { position = (x', y') } }

-- | Rotates current piece clockwise
rotateCurrent :: Game ()
rotateCurrent = do
  current <- gets currentPiece
  setCurrentPiece $ rotate current
 where
   rotate :: Block -> Block
   rotate block@(Block pos@(px,py) grid) =
     let (R.Z :. w :. h) = extent grid
         h' = w -- new height, flipped from old
         w' = h -- new width, flipped from old
         newsh = (R.Z :. w' :. h') -- new shape is has opposite dimensions of old
         -- make sure that the px and py values fit the piece on the board!
         permissibleX x
           | x < 0 = 0
           | x >= boardWidth - w' = boardWidth - w' - 1
           | otherwise = x
         permissibleY y
           | y < 0 = 0
           | y >= boardHeight - h' = boardHeight - h' -1
           | otherwise = y
         newpos = (permissibleX px, permissibleY py)
         -- shuffle pieces over to their new positions
         translateC (R.Z :. x :. y) = (R.Z :. (w - y - 1) :. x)
         newgrid = computeVectorS $ backpermute newsh translateC grid
     in block { position = newpos, guts = (newgrid) }


-- | Checks if the currentPiece needs to be set/cemented
checkPieceShouldBeSet :: Game Bool
checkPieceShouldBeSet = do
  current <- gets currentPiece
  brd <- gets board
  let (x,y) = position current
      grid = guts current
      currentsh@(R.Z :. w :. h) = extent grid
  if (y + h) >= boardHeight
    then return True -- Bottom of the board
         -- NOT (x+1) :. (y+1) because we're just looking down
         -- Looking down AND over causes segfaults, you jerk!
    else let boardSlice = extract (R.Z :. (x) :. (y+1)) currentsh brd
             -- Get the slice of the board one position and check them for collisions
         in return $ any id $ P.zipWith (bothJust) (toList boardSlice) (toList grid)
         -- in trace (show $ computeVectorS $ boardSlice) $ return False
 where
   bothJust (Just _) (Just _) = True
   bothJust _ _ = False


setPieceAndStartAnew :: Game ()
setPieceAndStartAnew = do 
  newNextBlock <- spawnBlock
  gs <- get
  let brd = board gs
      current = currentPiece gs
      nextCurrentBlock = nextPiece gs
      (px, py) = position current
      grid = guts current
      (R.Z :. w :. h) = extent grid
      insertPieceBlock lookupB spot@(R.Z :. x :. y) = 
        if any id [ x < px
                  , x >= (px + w)
                  , y < py
                  , y >= (py + h)
                  ]
        then lookupB spot -- This part is not in the overlapping current piece's grid
        else case grid ! (R.Z :. x - px :. y - py) of
          Nothing -> lookupB spot -- There wasn't anything in the current grid
          blk ->  blk -- There WAS something in the current grid - return that!
      newBoard = computeVectorS $ traverse brd id insertPieceBlock
--  trace (show newBoard) $ return ()
  put $ gs { board = newBoard
           , currentPiece = nextCurrentBlock { position = (4, 0) } -- on the board
           , nextPiece = newNextBlock { position = (12, 8) } -- off to the side
           }