module Game (tick, keyPress) where

--Only purely functional beyond this point

import Test.QuickCheck
import State(State)
import Control.Monad.Trans(liftIO)

tick :: State -> IO State
tick state = do
    return state

keyPress :: String -> State -> IO State
keyPress key state = do
    return state
