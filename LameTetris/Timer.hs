module LameTetris.Timer where

import Data.Word
import Control.Monad (when)
import Control.Monad.RWS
import Graphics.UI.SDL.Time
import LameTetris.Types

-- constants

-- | Max FPS
gameFPS :: Word32
gameFPS = 60

-- | Initial interval in milliseconds
initialInterval :: Word32
initialInterval = 1000

-- | How to determine the interval based on lines cleared
calculateInterval :: Word32 -> Word32
calculateInterval lines
  | lines < 100 = initialInterval - (75 * (lines `div` 10))
  | otherwise = (initialInterval `div` 2) - (40 * ((lines - 100) `div` 10))

-- | Pause timer
pause :: Game ()
pause = do
  st <- get -- Grab the game state
  let time = timer st
    in put $ st { timer = (time { paused = True }) }
       
-- | Unpause timer  
unpause :: Game ()
unpause = do
  st <- get -- Grab the game state
  let time = timer st
   in put $ st { timer = (time { paused = False }) }

-- | If it's too early to run the game loop again, just
  -- delay until it's time
waitUntilNextFrame :: Game ()
waitUntilNextFrame = do
  start <- fmap startedAt $ gets timer
  now <- liftIO getTicks
  let td = now - start
  when (td < minDelta) $
    liftIO $ delay (minDelta - td)
 where minDelta = 1000 `div` gameFPS

initialTimer :: IO Timer
initialTimer = do
  now <- liftIO getTicks
  return $ Timer { startedAt = now
                 , paused = False
                 , tsld = 0
                 , interval = initialInterval
                 }