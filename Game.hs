module Game (completeRows, rowReduce, tryRotate, tryMove, dropPiece) where

--Only purely functional beyond this point

import Test.QuickCheck
import Data.Maybe(fromJust, isJust)
import Debug.Trace
import Piece
import State as State
import Control.Monad.Trans(liftIO)


--rowReduce g = rowReduce' g 0 []
rowReduce :: [[Maybe Int]] -> (Int, [[Maybe Int]])
rowReduce g = (length cRows, ((newRows $ length cRows) ++ [g!!r | r<-[0..(length g)-1], r `notElem` cRows]))
	where cRows = completeRows g



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

dropPiece state
	| (tryMove state 0 1) == state = state
	| otherwise = dropPiece $ tryMove state 0 1
	

tryRotate :: State -> State
tryRotate state = 
	if canPlace g (rotateCW p) (State.position state)
		then State.setPiece state (rotatePieceCW $ fromJust (State.getPiece state)) (State.position state)
		else state
	where
		g = grid state
		p = piecee $ fromJust (activePiece state)

tryMove :: State -> Int -> Int -> State
tryMove state dx dy =
	if canPlace g p (x+dx, y+dy)
		then State.move state dx dy
		else state
	where
		g = grid state
		p = piecee $ fromJust (activePiece state)
		(x,y) = position state
