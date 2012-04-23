module LameTetris.Game where

import Control.Monad (when)
import Control.Monad.RWS
import Data.Array.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector

import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL

import LameTetris.Types
import qualified LameTetris.Types as LT
import LameTetris.Video


startBoard :: Board
startBoard = fromListVector (R.Z :. 10 :. 20) $ take 200 $ cells
  where
    cells :: [Cell]
    cells = Prelude.map (Just) $ cycle [I, J, L, O, S, T, LT.Z]

gameLoop :: Game ()
gameLoop = do
  event <- liftIO pollEvent
  when (event /= Quit) $ do drawBoard startBoard
                            gameLoop