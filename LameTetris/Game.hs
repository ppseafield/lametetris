{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             ScopedTypeVariables,
             OverlappingInstances #-}

module LameTetris.Game where

import System.Random
import Control.Monad (when)
import Control.Monad.RWS.Strict
import Data.Array.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector

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
                 return $ Block { position = (4, 1)
                                , guts = (getGuts btype)
                                }
-- | for use inside Game monad
spawnBlock :: Game Block                 
spawnBlock = liftIO spawnBlock'                   
  
-- | Empty board
startBoard :: Board
startBoard = fromListVector (R.Z :. 10 :. 20) $ replicate 200 Nothing

-- | Initial game state
initialGS :: IO GameState
initialGS = do time <- initialTimer
               frst <- spawnBlock'
               next <- spawnBlock'
               return $ GameState { lineNum = 0
                                  , timer = time
                                  , currentPiece = frst
                                  , nextPiece = next
                                  , board = startBoard
                                  }

-- | Handle events, then draw the game, then do it again (unless you quit)
gameLoop :: Game ()
gameLoop = do
  event <- handlePoll
  when (event /= Quit) $ do drawBoard startBoard
                            gameLoop

-- | Polls and polls until it can't poll no more!
handlePoll :: Game Event
handlePoll = do
  event <- liftIO pollEvent
  case event of Quit -> return Quit
                NoEvent -> return NoEvent
                _ -> handleEvent event >> handlePoll


-- | In any event, this is what you do:
handleEvent :: Event -> Game ()
handleEvent _ = return () -- TODO actually write this!