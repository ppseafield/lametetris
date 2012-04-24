module Main where

import Control.Monad.RWS.Strict
import Graphics.UI.SDL

import LameTetris.Types
import LameTetris.Video
import LameTetris.Game


main :: IO ()
main = withInit [InitEverything] $ do
  resc <- loadResources
  let gs = GameState 0 undefined undefined undefined startBoard
  (s, w) <- execRWST gameLoop resc gs
  ticks <- fmap fromEnum getTicks
  putStrLn $ "Average frame rate: " ++ (show $ w `div` (ticks `div` 1000))
  return ()