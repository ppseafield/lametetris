{-# LANGUAGE TypeOperators #-}

module LameTetris.Types where

import Data.Word
import Data.Monoid
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import qualified Data.Array.Repa as R
import Control.Monad.RWS.Strict

import Graphics.UI.SDL
import "SDL-ttf" Graphics.UI.SDL.TTF.Types


instance Monoid Int where
  mempty = 0
  mappend = (+)
  mconcat = sum

-- | Game monad
  -- Resources = SDL Surfaces/fonts
  -- Int = number of frames
type Game = RWST Resources Int GameState IO

-- | Regulates time
data Timer =
  Timer { startedAt :: Word32
        , tsld :: Word32 -- ^ time since last drop
        , interval :: Word32 -- ^ How long it takes for a piece to drop
        } deriving (Show)

-- | Grid cell type
--   Any cell can be either empty, or filled with some block type.
--   Each block type corresponds to a specific color block.
--   Empty cells are represented by `Nothing`, while occupied cells
--   are something like `Just L`.
type Cell = Maybe BlockType

-- | Tetris block type
data BlockType = I | J | L | O | S | T | Z
               deriving (Show, Eq, Ord, Enum, Bounded)

-- | 2D Coordinate
type Point = (Int, Int)

-- | A 2D array of cells
type Grid = R.Array V DIM2 Cell

-- | The Board is a big grid!
type Board = Grid

-- | How an active block is described
data Block = Block { position :: Point
                   , guts :: Grid
                   } deriving (Show)


-- | Contains all SDL resources necessary to render the game
data Resources =
  Resources { mainScreen :: Surface
            , tileSet :: Surface
            , font :: Font
            }

-- | Describes everything that's going on in the game right now
data GameState =
  GameState { lineNum :: Word32 -- ^ how many lines have been eliminated
            , timer :: Timer -- ^ regulates time
            , currentPiece :: Block -- ^ current dropping piece
            , nextPiece :: Block -- ^ next piece to be generated
            , board :: Board
            , linesText :: Surface
            , gameOver :: Bool
            } deriving (Show)