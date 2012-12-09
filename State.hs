module State (State, newState, emptyGrid, printState, getPiece, setPiece, move, getPosition) where

import Piece

data State = State {grid :: Grid
                   , points :: Int
                   , activePiece :: Maybe Piece
                   , position :: Pos
                   } deriving (Show, Eq)

data Grid = Grid { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

type Pos = (Int,Int)


gameWidth = 10
gameHeight = 10


newState :: State
newState = 
    State {grid=emptyGrid, points=0, activePiece=(Just randomPiece), position=(0,0)}

getPiece :: State -> Maybe Piece
getPiece (State grid points piece pos) = piece

setPiece :: State -> Piece -> Pos -> State
setPiece (State grid points _  _) piece pos = State grid points (Just piece) pos

-- FIXME: check for collisions / allowed moves
move :: State -> Int -> Int -> State
move (State grid points activePiece (x,y)) dx dy = State grid points activePiece (x + dx, y + dy)

getPosition (State grid points piece pos) = pos

emptyGrid :: Grid
emptyGrid = 
    Grid [[Nothing | _ <- [1..gameWidth] ] | _ <- [1..gameHeight]]

clear = putStr "\ESC[2J"

-- printState prints a representation of the state on the screen
printState :: State -> IO ()
printState (State grid points activePiece position) = do
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
