module Game (tick, keyPress) where

--Only purely functional beyond this point

import Test.QuickCheck
import Data.Maybe(fromJust)
import Debug.Trace
import Piece
import State as State
import Control.Monad.Trans(liftIO)

tick :: State -> IO State
--tick (State.State g p Nothing _) = undefined --wat?
tick state = do
	tick' (State.grid state) (piecee (fromJust $ State.activePiece state)) (State.position state) state
	
tick' :: Grid -> [[Maybe Int]] -> Pos -> State -> IO State
tick' board piece (x,y) state = do
	if State.canPlace board piece (x,(y+1))
		then return $ State.move state 0 1
		else return state --todo new piece, merge piece with grid

-- FIXME: this state keeping is messed up
keyPress :: String -> State -> IO State
keyPress key state | trace ("keyPress (" ++ show key ++ ", ...)") False = undefined
                   | otherwise = do
    case key of
		"Up"    -> return $ State.setPiece state (rotatePieceCW $ fromJust (State.getPiece state)) (State.position state)
		"Down"  -> tick state
		"Left"  -> return $ tryMove state (-1) 0
		"Right" -> return $ tryMove state 1 0
		_       -> return state


tryMove :: State -> Int -> Int -> State
tryMove state dx dy =
	if canPlace g p (x+dx, y+dy)
		then State.move state dx dy
		else state
	where
		g = grid state
		p = piecee $ fromJust (activePiece state)
		(x,y) = position state
