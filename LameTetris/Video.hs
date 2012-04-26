{-# LANGUAGE TypeOperators #-}

module LameTetris.Video where

import Data.Word
import Data.Array.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import Control.Monad (mapM_, when)
import Control.Monad.RWS.Strict

import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Image

import LameTetris.Types
import qualified LameTetris.Types as LT
import LameTetris.Utils


-- | Background color pixel 
bgPixel = Pixel 0x201e1e


{- Drawing functions -}

-- | Draws the entire board
  -- (does not include border)
drawBoard :: Board -> Game ()
drawBoard board = do screen <- asks mainScreen
                     currentb <- gets currentPiece
                     nextb <- gets nextPiece
                     drawStaticBoardParts
                     drawBlock currentb
                     drawBlock nextb
                     mapM_ drawCoord allCoords
                     liftIO $ SDL.flip screen
                     tell 1 -- frame increment
                     
  where
    allCoords = [(x,y) | x <- [0..(boardWidth - 1)],
                         y <- [0..(boardHeight - 1)] ]
    drawCoord (x,y) = drawCell x y $ board ! (R.Z :. x :. y)


-- | Draws an individual cell
drawCell :: Int -> Int -> Cell -> Game ()
drawCell x y cell = do
  screen <- asks mainScreen
  tiles <- asks tileSet

  liftIO $ case cell of
             Just btype -> blitSurface tiles (getBlockRect btype) screen $ location x y
             Nothing -> return False -- blitSurface tiles blankRect screen $ location x y
  return ()
 where
   -- | Empty board sqare
   blankRect = rpoint 32 32


-- | Draw a whole piece
drawBlock :: Block -> Game ()
drawBlock block = do screen <- asks mainScreen
                     tiles <- asks tileSet
                     mapM_ drawCoord allCoords
  where blockGuts = guts block
        (R.Z :. w :. h)  = extent blockGuts
        (startx, starty) = position block
        allCoords = [(x, y) | x <- [0 .. w - 1],
                              y <- [0 .. h - 1] ]
        drawCoord (x,y) = drawCell (startx + x) (starty + y) $ blockGuts ! (R.Z :. x :. y)
               


-- | Draws everything that never changes
drawStaticBoardParts :: Game ()
drawStaticBoardParts = do
  screen <- asks mainScreen
  tiles <- asks tileSet

  liftIO $ do fillRect screen Nothing bgPixel
              -- easy blitting function
              let blitTile clipRect = blitSurface tiles clipRect screen . (uncurry location)
              
              -- Top Left
              blitTile topLeft ((-1), (-1))
              -- Top Sides
              mapM_ (blitTile topSide) topCoords
              -- Top Right
              blitTile topRight (10, (-1))
              -- Right Sides
              mapM_ (blitTile rightSide) rightCoords
              -- Bottom Right
              blitTile bottomRight (10, 20)
              -- Bottom Sides
              mapM_ (blitTile bottomSide) bottomCoords
              -- Bottom Left
              blitTile bottomLeft ((-1), 20)
              -- Left Sides
              mapM_ (blitTile leftSide) leftCoords
              
              -- title text
              blitSurface tiles (Just $ Rect 32 160 64 32) screen (rpoint 450 100)
              -- next piece
              blitSurface tiles (Just $ Rect 56 199 41 15) screen (rpoint 465 220)
              -- lines
              blitSurface tiles (Just $ Rect 6 197 44 17)  screen (rpoint 465 500)

              return ()
              
 where
   -- list of coordinates for each side
   topCoords = [ (x, (-1)) | x <- [0 .. boardWidth - 1] ]
   rightCoords = [ (10, y) | y <- [0 .. boardHeight - 1] ]
   bottomCoords = [ (x, 20) | x <- [0 .. boardWidth -1] ]
   leftCoords = [ ((-1), y) | y <- [0 .. boardHeight -1] ]
   -- Sections of the tileset for the board side
   topLeft = rpoint 0 0
   topSide = rpoint 32 0
   topRight = rpoint 64 0
   rightSide = rpoint 64 32
   bottomRight = rpoint 64 64
   bottomSide = rpoint 32 64
   bottomLeft = rpoint 0 64
   leftSide = rpoint 0 32
              

initScreenFlags :: [SurfaceFlag]
initScreenFlags = [HWSurface, HWAccel, DoubleBuf]
-- initScreenFlags = [SWSurface] 

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = do
  pxl <- mapRGB (surfaceGetPixelFormat surface) r g b
  setColorKey surface [SrcColorKey, RLEAccel] pxl
  return surface

loadResources :: IO Resources
loadResources = do
  screen <- setVideoMode 600 768 32 initScreenFlags
  setCaption "Lame Tetris!" []
  tiles <- loadImage "data/tiles.png" Nothing
  return $ Resources { mainScreen = screen
                     , tileSet = tiles
                     , font = undefined
                     }
