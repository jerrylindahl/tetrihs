module Main (main) where

--Lets keep all the ugly IO stufs in here.

import Test.QuickCheck
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Control.Concurrent.MVar
import Game as Game
import State as State


handleKeyPress key state = do
    s <- takeMVar state
    newState <- Game.keyPress key s
    putMVar state newState
    
gameLoop state = do
    s <- takeMVar state
    a <- Game.tick s
    State.printState a
    putMVar state a

main = do
    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll window
    
    --game state!
    a<- return State.newState
    state <- newMVar a
    
    
    window `on` keyPressEvent $ tryEvent $ do
        key <- eventKeyName
        liftIO $ handleKeyPress key state
    
    --timeout value seems to not work?
    timeoutAdd (gameLoop state >> return True) 1000
   
    mainGUI

