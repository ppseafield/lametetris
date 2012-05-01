module Main where

import Control.Monad.RWS.Strict
import System.Exit (exitFailure)

import Graphics.UI.SDL
import qualified Graphics.UI.SDL.TTF.General as TTFG

import LameTetris.Types
import LameTetris.Video
import LameTetris.Game


main :: IO ()
main = withInit [InitEverything] $ do

  ttfWorks <- TTFG.init
  when (not ttfWorks) $ do
    putStrLn "Couldn't start SDL_ttf. Sad day. :("
    exitFailure
  
  resc <- loadResources
  gs <- initialGS (font resc) -- font for rendering "0" lines text
  execRWST gameLoop resc gs
  -- (s, w) <- execRWST gameLoop resc gs
  -- ticks <- fmap fromEnum getTicks
  -- putStrLn $ "Average frame rate: " ++ (show $ w `div` (ticks `div` 1000))
  -- putStrLn $ "GameState: \n" ++ (show s)
  putStrLn "Thanks for playing LameTetris!"
