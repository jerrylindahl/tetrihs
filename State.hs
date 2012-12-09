module State (State, newState, emptyGrid, printState) where

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


-- printState prints a representation of the state on the screen
printState :: State -> IO ()
printState (State grid points activePiece) =
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
