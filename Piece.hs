module Piece (Piece, piecee, randomPieces, rotateCW, rotatePieceCW, getCoords) where

import Data.List(transpose)
import System.Random

data Piece = Piece {piecee :: [[Maybe Int]]}
    deriving (Show, Eq)

type Pos = (Int,Int)


-- Generate an infinite list of random pieces
randomPieces :: StdGen -> [Piece]
randomPieces gen = map (pieces !!) $ randomRs (0, length pieces - 1) gen

-- Rotate 90° to the right
rotateCW :: [[a]] -> [[a]]
rotateCW = transpose . reverse

rotatePieceCW :: Piece -> Piece
rotatePieceCW (Piece p) = Piece $ rotateCW p

-- Rotating 4 times should get the original thing back
prop_rotatePieceCW p = p == (rotatePieceCW $ rotatePieceCW $ rotatePieceCW $ rotatePieceCW p)

-- Get coordinates where somethings needs to be drawn
getCoords :: Piece -> [Pos]
getCoords (Piece p) = [ (x,y) | (y,r) <- zip [0..] p, (x,n) <- zip [0..] r, n /= Nothing ]

pieces = [pieceI, pieceJ, pieceL, pieceO, pieceS, pieceT, pieceZ]

pieceI :: Piece
pieceI =
    Piece
      [ [Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Just 1 ,Just 1 ,Just 1 ,Just 1 ]
      , [Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing]
      ]

pieceJ :: Piece
pieceJ =
    Piece
      [ [Just 1 ,Nothing,Nothing]
      , [Just 1 ,Just 1 ,Just 1 ]
      , [Nothing,Nothing,Nothing]
      ]

pieceL :: Piece
pieceL =
    Piece
      [ [Nothing,Nothing,Just 1 ]
      , [Just 1 ,Just 1 ,Just 1 ]
      , [Nothing,Nothing,Nothing]
      ]

pieceO :: Piece
pieceO =
    Piece
      [ [Just 1 ,Just 1 ]
      , [Just 1 ,Just 1 ]
      ]

pieceS :: Piece
pieceS =
    Piece
      [ [Nothing,Just 1 ,Just 1 ]
      , [Just 1 ,Just 1 ,Nothing]
      ]

pieceT :: Piece
pieceT =
    Piece
      [ [Nothing,Just 1 ,Nothing]
      , [Just 1 ,Just 1 ,Just 1 ]
      , [Nothing,Nothing,Nothing ]
      ]

pieceZ :: Piece
pieceZ =
    Piece
      [ [Just 1 ,Just 1 ,Nothing]
      , [Nothing,Just 1 ,Just 1 ]
      ]
