module LameTetris.Types where

--import Data.Array.IO

import Graphics.UI.SDL


data BlockType = I | J | L | O | S | T | Z
               deriving (Show, Eq, Ord, Enum, Bounded)

-- | 2D Coordinate
type Point = (Int, Int)

newtype Block = Block BlockType Point (IOArray Point 

-- | Contains all SDL resources necessary to render the game
data Resources =
  Resources { mainScreen :: Surface
            , tileSet :: Surface
            }

data TetrisState =
  TetrisState { }