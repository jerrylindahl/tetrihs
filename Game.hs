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

-- FIXME: this state keeping is messed up
keyPress :: String -> State -> IO State
keyPress key state | trace ("keyPress (" ++ show key ++ ", ...)") False = undefined
                   | otherwise = do
    case key of
		"Up"    -> return $ State.setPiece state (rotatePieceCW $ fromJust (State.getPiece state)) (State.getPosition state)
		"Down"  -> tick state
		"Left"  -> return $ State.move state (-1) 0
		"Right" -> return $ State.move state 1 0
		_       -> return state


