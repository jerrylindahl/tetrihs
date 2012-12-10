module State (State, Pos, newState, emptyGrid, printState, getPiece, setPiece, move, getPosition) where

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


--canPlace example [[Nothing, Nothing, Just 3],[Just 3, Just 3, Nothing],[Just 3,Just 2,Nothing]] (0,0)
--takes a grid and tries to place Piece at Pos, return if it works
canPlace :: Grid -> [[Maybe Int]] -> Pos -> Bool
canPlace grid p pos = canPlace' p 0
	where
		canPlace' [] _ = True
		canPlace' (r:rs) rowCount = (canPlaceRow grid pos rowCount 0 r) && (canPlace' rs (rowCount+1))

--canPlaceRow example (1,0) 0 0 [Nothing, Just 1, Just 3]
canPlaceRow :: Grid -> Pos -> Int -> Int -> [Maybe Int] -> Bool
canPlaceRow _ _ _ _ [] = True
canPlaceRow (Grid rows) (x,y) rowCount colCount (Nothing:ps)
	= canPlaceRow (Grid rows) (x,y) rowCount (colCount+1) ps
canPlaceRow (Grid rows) (x,y) rowCount colCount (p:ps)
	= rows!!(y+rowCount)!!(x+colCount)==Nothing && canPlaceRow (Grid rows) (x,y) rowCount (colCount+1) ps

gridEmpty :: Grid -> Pos -> Bool
gridEmpty (Grid rows) (x,y) = rows!!x!!y == Nothing


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


--actually a sudoku but works for testing purposes :)
example :: Grid
example =
    Grid
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]
