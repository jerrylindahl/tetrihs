module Piece (Piece, piecee, colour, randomPieces, rotateCW, rotatePieceCW, getCoords, Colour, colourRGB) where

import Data.List(transpose)
import System.Random

data Piece = Piece { piecee :: [[Maybe Int]], colour :: Colour }
    deriving (Show, Eq)

type Pos = (Int,Int)

data Colour = Red | Blue | Orange | Yellow | Magneta | Cyan | Green
    deriving (Eq, Show)

colourRGB :: Colour -> (Double, Double, Double)
colourRGB c
    | c == Red     = (1, 0,       0)
    | c == Blue    = (0, 0,       1)
    | c == Orange  = (1, 165/255, 0)
    | c == Yellow  = (1, 1,       0)
    | c == Magneta = (1, 0,       1)
    | c == Cyan    = (0, 1,       1)
    | c == Green   = (0, 1,       0)

-- Generate an infinite list of random pieces
randomPieces :: StdGen -> [Piece]
randomPieces gen = map (pieces !!) $ randomRs (0, length pieces - 1) gen

-- Rotate 90° to the right
rotateCW :: [[a]] -> [[a]]
rotateCW = transpose . reverse

rotatePieceCW :: Piece -> Piece
rotatePieceCW (Piece p c) = Piece (rotateCW p) c

-- Rotating 4 times should get the original thing back
prop_rotatePieceCW p = p == (rotatePieceCW $ rotatePieceCW $ rotatePieceCW $ rotatePieceCW p)

-- Get coordinates where somethings needs to be drawn
getCoords :: Piece -> [Pos]
getCoords (Piece p _) = [ (x,y) | (y,r) <- zip [0..] p, (x,n) <- zip [0..] r, n /= Nothing ]

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
      Red

pieceJ :: Piece
pieceJ =
    Piece
      [ [Just 1 ,Nothing,Nothing]
      , [Just 1 ,Just 1 ,Just 1 ]
      , [Nothing,Nothing,Nothing]
      ]
      Blue

pieceL :: Piece
pieceL =
    Piece
      [ [Nothing,Nothing,Just 1 ]
      , [Just 1 ,Just 1 ,Just 1 ]
      , [Nothing,Nothing,Nothing]
      ]
      Orange

pieceO :: Piece
pieceO =
    Piece
      [ [Just 1 ,Just 1 ]
      , [Just 1 ,Just 1 ]
      ]
      Yellow

pieceS :: Piece
pieceS =
    Piece
      [ [Nothing,Just 1 ,Just 1 ]
      , [Just 1 ,Just 1 ,Nothing]
      ]
      Magneta

pieceT :: Piece
pieceT =
    Piece
      [ [Nothing,Just 1 ,Nothing]
      , [Just 1 ,Just 1 ,Just 1 ]
      , [Nothing,Nothing,Nothing ]
      ]
      Cyan

pieceZ :: Piece
pieceZ =
    Piece
      [ [Just 1 ,Just 1 ,Nothing]
      , [Nothing,Just 1 ,Just 1 ]
      ]
      Green
