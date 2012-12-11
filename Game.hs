module Game (tick, keyPress, completeRows) where

--Only purely functional beyond this point

import Test.QuickCheck
import Data.Maybe(fromJust, isJust)
import Debug.Trace
import Piece
import State as State
import Control.Monad.Trans(liftIO)

tick :: State -> IO State
--tick (State.State g p Nothing _) = undefined --wat?
tick state = do
	tick' (State.grid state) (fromJust $ State.activePiece state) (State.position state) state
	
tick' :: Grid -> Piece -> Pos -> State -> IO State
tick' board piece (x,y) state = do
	if State.canPlace board (piecee piece) (x,(y+1))
		then return $ State.move state 0 1
		else return $ pieceDone board 0 piece (x,y) state

--new piece, merge piece with grid
-- <<<<<<< HEAD
--pieceDone :: Grid -> Int -> [[Maybe Int]] -> Pos -> State -> State
--pieceDone g points ap pos state =
	--createState (makeGrid newnewg) (points+newpoints) (activePiece state) pos
	--where 
		--(newpoints, newnewg) = rowReduce (rows newg)
		--newg = addPieceToGrid ap g --Kalev add the merg thing here

-- =======
pieceDone :: Grid -> Int -> Piece -> Pos -> State -> State
pieceDone g points ap (x,y) state
    | y == 0    = error "End of game" -- End of the game, crash for now
    | otherwise =
	createState (makeGrid newnewg) points (Just randomPiece) (4,0)
	where 
		(newpoints, newnewg) = rowReduce (rows newg)
		newg = addPieceToGrid ap g (x,y)


--rowReduce g = rowReduce' g 0 []
rowReduce :: [[Maybe Int]] -> (Int, [[Maybe Int]])
rowReduce g = (length cRows, ((newRows $ length cRows) ++ [g!!r | r<-[0..(length g)-1], r `notElem` cRows]))
	where cRows = completeRows g

newRows :: Int -> [[Maybe Int]]
newRows n = (replicate n (replicate 10 Nothing))

--rowReduce' [] p ogs = (p, ogs) --todo add new rows
--rowReduce' g:gs p ogs = rowReduce' gs ogs:

--Game.completeRows (rows emptyGrid)
completeRows :: [[Maybe Int]] -> [Int]
completeRows g = completeRows' g [] 0

completeRows' :: [[Maybe Int]] -> [Int] -> Int -> [Int]
completeRows' [] rows i = rows
completeRows' (g:gs) rows i 
	| all (isJust) g 	= completeRows' gs (i:rows) (i+1)
	| otherwise 		= completeRows' gs rows (i+1)

-- FIXME: this state keeping is messed up
keyPress :: String -> State -> IO State
keyPress key state | trace ("keyPress (" ++ show key ++ ", ...)") False = undefined
                   | otherwise = do
    case key of
		"Up"    -> return $ tryRotate state
		"Down"  -> tick state
		"Left"  -> return $ tryMove state (-1) 0
		"Right" -> return $ tryMove state 1 0
		_       -> return state

--copy paste, REFACTOR :)
tryRotate :: State -> State
tryRotate state = 
	if canPlace g (rotateCW p) (State.position state)
		then State.setPiece state (rotatePieceCW $ fromJust (State.getPiece state)) (State.position state)
		else state
	where
		g = grid state
		p = piecee $ fromJust (activePiece state)
		(x,y) = position state



tryMove :: State -> Int -> Int -> State
tryMove state dx dy =
	if canPlace g p (x+dx, y+dy)
		then State.move state dx dy
		else state
	where
		g = grid state
		p = piecee $ fromJust (activePiece state)
		(x,y) = position state
