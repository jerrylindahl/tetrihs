module State (State, Grid, Pos, newRows, points, createState, rows, makeGrid, grid, canPlace, activePiece, addPieceToGrid, position, newState, emptyGrid, printState, getPiece, setPiece, move,  getGridCoords) where


import Piece
import Debug.Trace

data State = State {grid :: Grid
                   , points :: Int
                   , activePiece :: Maybe Piece
                   , position :: Pos
                   } deriving (Show, Eq)

data Grid = Grid { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

type Pos = (Int,Int)


gameWidth = 10
gameHeight = 20

createState :: Grid -> Int -> Maybe Piece -> Pos -> State
createState board p ap pos =
	State {grid=board, points=p, activePiece=ap, position=pos}

makeGrid :: [[Maybe Int]] -> Grid
makeGrid g = (Grid g)

newState :: State
newState = 
    State {grid=emptyGrid, points=0, activePiece=(Just randomPiece), position=(4,0)}

getPiece :: State -> Maybe Piece
getPiece (State grid points piece pos) = piece

setPiece :: State -> Piece -> Pos -> State
setPiece (State grid points _  _) piece pos = State grid points (Just piece) pos

-- FIXME: check for collisions / allowed moves
move :: State -> Int -> Int -> State
move (State grid points activePiece (x,y)) dx dy = State grid points activePiece (x + dx, y + dy)

-- Get coordinates where somethings needs to be drawn
-- FIXME: copy-paste from Piece.hs
getGridCoords :: Grid -> [Pos]
getGridCoords (Grid g) = [ (x,y) | (y,r) <- zip [0..] g, (x,n) <- zip [0..] r, n /= Nothing ]


--takes a grid and tries to place Piece at Pos, return if it works
canPlace :: Grid -> [[Maybe Int]] -> Pos -> Bool
canPlace grid p pos = canPlace' p 0 
	where
		canPlace' [] _ = True
		canPlace' (r:rs) rowCount = (canPlaceRow grid pos rowCount 0 r) && (canPlace' rs (rowCount+1))

canPlaceRow :: Grid -> Pos -> Int -> Int -> [Maybe Int] -> Bool
canPlaceRow _ _ _ _ [] = True
canPlaceRow (Grid rows) (x,y) rowCount colCount (Nothing:ps)
	= canPlaceRow (Grid rows) (x,y) rowCount (colCount+1) ps
canPlaceRow (Grid rows) (x,y) rowCount colCount (p:ps) =
	y+rowCount < gameHeight &&
	x+colCount >= 0  &&
	x+colCount < gameWidth &&
	rows!!(y+rowCount)!!(x+colCount)==Nothing && 
	canPlaceRow (Grid rows) (x,y) rowCount (colCount+1) ps
	
gridEmpty :: Grid -> Pos -> Bool
gridEmpty (Grid rows) (x,y) = rows!!x!!y == Nothing


addPieceToGrid piece (Grid grid) offset =
        Grid $ replaceMultipleBlocks grid offset (Just 42) coords
        where
                coords = getCoords piece

replaceMultipleBlocks grid (x,y) newBlock ((x1,y1):coords)
    | coords == [] = updatedGrid
    | otherwise    = replaceMultipleBlocks updatedGrid (x,y) newBlock coords
        where
                updatedGrid = replaceBlock grid newBlock (x+x1, y+y1)

replaceBlock grid newBlock (x,y)
    | trace ("addBlock (..., " ++ show x ++ "," ++ show y ++ ")") False = undefined
    | otherwise =
        rowsBefore ++ newRow:rowsAfter
        where
                (rowsBefore,row:rowsAfter) = splitAt y grid
                (blocksBefore,_:blocksAfter) = splitAt x row
                newRow = blocksBefore ++ newBlock:blocksAfter


emptyGrid :: Grid
emptyGrid = 
    Grid (replicate gameHeight (replicate gameWidth Nothing))

newRows :: Int -> [[Maybe Int]]
newRows n = (replicate n (replicate gameWidth Nothing))

clear = putStr "\ESC[2J"

-- printState prints a representation of the state on the screen
printState :: State -> IO ()
printState (State grid points activePiece position) = do
    clear
    putStrLn "-----"
    putStrLn ("Points: " ++ (show points))
    putStrLn "-----"
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


--proper width and height
example :: Grid
example =
    Grid
      [ [Just 9,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 9,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 9,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 9,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 9,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 9,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Just 3, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 1,Just 1,Just 1, Just 1,Just 1,Just 1,Just 1,Just 1, Just 1, Just 1]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Just 3]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 2,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Nothing]
      , [Just 4,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing, Nothing, Just 1]
      ]


--actually a sudoku but works for testing purposes :)
example2 :: Grid
example2 =
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


