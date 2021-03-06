module State (State, Grid, Pos, newRows, points, createState, rows, makeGrid, grid, canPlace, activePiece, addPieceToGrid, position, newState, emptyGrid, printState, getPiece, setPiece, move,  getGridCoords) where


import Data.Maybe(fromJust)
import Piece
import Debug.Trace

data State = State {grid :: Grid
                   , points :: Int
                   , activePiece :: Maybe Piece
                   , position :: Pos
                   } deriving (Show, Eq)

data Grid = Grid { rows :: [[Maybe Colour]] }
 deriving ( Show, Eq )

type Pos = (Int,Int)


gameWidth = 10
gameHeight = 20

createState :: Grid -> Int -> Maybe Piece -> Pos -> State
createState board p ap pos =
	State {grid=board, points=p, activePiece=ap, position=pos}

makeGrid :: [[Maybe Colour]] -> Grid
makeGrid g = (Grid g)

newState :: Maybe Piece -> State
newState ap =
    State {grid=emptyGrid, points=0, activePiece=ap, position=(4,0)}

getPiece :: State -> Maybe Piece
getPiece (State grid points piece pos) = piece

setPiece :: State -> Piece -> Pos -> State
setPiece (State grid points _  _) piece pos = State grid points (Just piece) pos

-- FIXME: check for collisions / allowed moves
move :: State -> Int -> Int -> State
move (State grid points activePiece (x,y)) dx dy = State grid points activePiece (x + dx, y + dy)

-- Get coordinates where somethings needs to be drawn
-- FIXME: copy-paste from Piece.hs
getGridCoords :: Grid -> [(Pos, Colour)]
getGridCoords (Grid g) = [ ((x,y), fromJust n) | (y,r) <- zip [0..] g, (x,n) <- zip [0..] r, n /= Nothing ]


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
        Grid $ replaceMultipleBlocks grid offset (Just $ colour piece) coords
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

newRows :: Int -> [[Maybe Colour]]
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

printHelp :: [[Maybe Colour]] -> IO ()
printHelp [] = return ()
printHelp (x:xs) = do
    printRow x
    printHelp xs

printRow :: [Maybe Colour] -> IO ()
printRow [] =
    putStr "\n"
printRow (Just x:xs) = do
    putStr "*"
    printRow xs
printRow (Nothing:xs) = do
    putStr "."
    printRow xs
