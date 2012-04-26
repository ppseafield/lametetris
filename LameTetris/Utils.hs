{-# LANGUAGE TypeOperators #-}

module LameTetris.Utils where

import Data.Word
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import qualified Data.Array.Repa as R
import Control.Monad.RWS.Strict

import Graphics.UI.SDL

import LameTetris.Types
import qualified LameTetris.Types as LT


ifM :: (Monad m) => m Bool -> m () -> m () -> m ()
ifM condition action1 action2 = do cond <- condition
                                   if cond
                                     then action1
                                     else action2

-- | Board dimensions
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- | width / height of a block's tile
blockWidth, blockHeight :: Int
blockWidth = 32
blockHeight = 32

-- | X, Y coordinates where the board should start to be drawn
boardStartX, boardStartY :: Int
boardStartX = 50
boardStartY = 70

-- | Used in blitting as a coordinate
rpoint :: Int -> Int -> Maybe Rect
rpoint x y = Just $ Rect x y 32 32

-- | Takes a position in the Array (e.g. (2,11)) and turns it into
  -- a real coordinate on the screen
location :: Int -> Int -> Maybe Rect
location x y = Just $ Rect ((x * blockWidth) + boardStartX)
                           ((y * blockHeight) + boardStartY)
                           0
                           0

{- Setter functions -}
setLineNum :: Word32 -> Game ()
setLineNum linum = do gs <- get
                      put $ gs { lineNum = linum }

setTimer :: Timer -> Game ()
setTimer time = do gs <- get
                   put $ gs { timer = time }

setCurrentPiece :: Block -> Game ()
setCurrentPiece piece = do gs <- get
                           put $ gs { currentPiece = piece }

setNextPiece :: Block -> Game ()
setNextPiece npiece = do gs <- get
                         put $ gs { nextPiece = npiece }

setBoard :: Board -> Game ()
setBoard brd = do gs <- get
                  put $ gs { board = brd }


{- BlockType functions -}

-- | Produces a rect for the tileset surface
  -- given a specific block type
getBlockRect :: BlockType -> Maybe Rect
getBlockRect I = rpoint 0 96
getBlockRect J = rpoint 32 96
getBlockRect L = rpoint 64 96
getBlockRect O = rpoint 0 128
getBlockRect S = rpoint 32 128
getBlockRect T = rpoint 64 128
getBlockRect LT.Z = rpoint 0 160


-- | Given a block type, produce the "guts" of a block
-- | Remember, these fill in top down, THEN left -> right
getGuts :: BlockType -> Grid
getGuts I = fromListVector (R.Z :. 4 :. 1) $ -- right
              [ Just I
              , Just I
              , Just I
              , Just I
              ]

getGuts J = fromListVector (R.Z :. 2 :. 3) $ -- right
              [ Nothing, Nothing, Just J
              , Just J, Just J, Just J
              ]

getGuts L = fromListVector (R.Z :. 2 :. 3) $ -- right
              [ Just L, Just L, Just L,
                Nothing, Nothing, Just L
              ]

getGuts O = fromListVector (R.Z :. 2 :. 2) $ -- right
              [ Just O, Just O
              , Just O, Just O
              ]

getGuts S = fromListVector (R.Z :. 3 :. 2) $ -- right
              [ Nothing, Just S
              , Just S, Just S
              , Just S, Nothing
              ]

getGuts T = fromListVector (R.Z :. 3 :. 2) $ -- right
              [ Just T, Nothing
              , Just T, Just T
              , Just T, Nothing
              ]

getGuts LT.Z = fromListVector (R.Z :. 3 :. 2) $
              [ Just LT.Z, Nothing
              , Just LT.Z, Just LT.Z
              , Nothing,  Just LT.Z
              ]