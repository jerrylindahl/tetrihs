module Game (tick, keyPress) where

--Only purely functional beyond this point

import Test.QuickCheck
import Data.Maybe(fromJust)
import Debug.Trace
import Piece
import State as State
import Control.Monad.Trans(liftIO)

tick :: State -> IO State
tick state = do
    return $ State.move state 0 1

-- FIXME: don't replace the Piece on the Board with every keypress
-- FIXME: this state keeping is messed up
keyPress :: String -> State -> IO State
keyPress key state | trace ("keyPress (" ++ show key ++ ", ...)") False = undefined
                   | otherwise = do
    if key == "a" then
		return $ State.setPiece state (rotatePieceCW $ fromJust (State.getPiece state)) (State.getPosition state)
	else
		if key == "s" then
			return $ State.setPiece state (rotatePieceACW $ fromJust (State.getPiece state)) (State.getPosition state)
		else
			return state


