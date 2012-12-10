module Piece (Piece, randomPiece, rotatePieceCW, rotatePieceACW, getCoords) where

import Data.List(transpose)

data Piece = Piece [[Maybe Int]]
    deriving (Show, Eq)

type Pos = (Int,Int)


-- Choose a random piece
-- FIXME: currently not very random
randomPiece :: Piece
randomPiece = pieceJ

-- Rotate 90° to the right
rotateCW :: [[a]] -> [[a]]
rotateCW = transpose . reverse

rotatePieceCW :: Piece -> Piece
rotatePieceCW (Piece p) = Piece $ rotateCW p

-- Rotating 4 times should get the original thing back
prop_rotatePieceCW p = p == (rotatePieceCW $ rotatePieceCW $ rotatePieceCW $ rotatePieceCW p)

rotatePieceACW = rotatePieceCW . rotatePieceCW . rotatePieceCW

-- Get coordinates where somethings needs to be drawn
getCoords :: Piece -> [Pos]
getCoords (Piece p) = [ (x,y) | (y,r) <- zip [0..] p, (x,n) <- zip [0..] r, n /= Nothing ]


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
