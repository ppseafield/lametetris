module Main where

import Control.Monad.RWS
import Graphics.UI.SDL

import LameTetris.Types
import LameTetris.Video
import LameTetris.Game


main :: IO ()
main = withInit [InitEverything] $ do
  resc <- loadResources
  let gs = GameState 0 undefined undefined undefined startBoard
  execRWST gameLoop resc gs
  return ()