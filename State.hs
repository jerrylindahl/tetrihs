module State (State, newState, emptyGrid, printState) where

import Data.List(transpose)

data State = State {grid :: Grid
                   , points :: Int
                   , activePiece :: Maybe Piece
                   } deriving (Show, Eq)

data Grid = Grid { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

--4 by 4 grid or 3 by 3
data Piece = Piece [[Maybe Int]]
 deriving (Show, Eq)


gameWidth = 10
gameHeight = 10


newState :: State
newState = 
    State {grid=emptyGrid, points=0, activePiece=Nothing}

emptyGrid :: Grid
emptyGrid = 
    Grid [[Nothing | _ <- [1..gameWidth] ] | _ <- [1..gameHeight]]

clear = putStr "\ESC[2J"

-- printState prints a representation of the state on the screen
printState :: State -> IO ()
printState (State grid points activePiece) = do
    clear
    printHelp (rows grid)

printHelp :: [[Maybe Int]] -> IO ()
printHelp [] = return ()
printHelp (x:xs) = do
    printRow x
    printHelp xs

printRow :: [Maybe Int] -> IO ()
printRow [] =
    putStr "\n"
printRow (Just x:xs) = do
    putStr $ show x
    printRow xs
printRow (Nothing:xs) = do
    putStr "."
    printRow xs

--perhaps
--EX 3x3
-- | -XX
-- | XX-
-- | ---

--EX 4x4
-- | --X-
-- | --X-
-- | --X-
-- | -XX-


-- Rotate 90° to the right
rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

rotatePiece :: Piece -> Piece
rotatePiece (Piece p) = Piece $ rotate p

-- Rotating 4 times should get the original thing back
prop_rotatePiece p = p == (rotatePiece $ rotatePiece $ rotatePiece $ rotatePiece p)

printPiece :: Piece -> IO ()
printPiece (Piece p) = printHelp p


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
