module Main (main) where

--Lets keep all the ugly IO stufs in here.

import Test.QuickCheck
import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Game as Game
import Piece
import State as State
import Debug.Trace

blockSize = 40
canvasWidth = 300
canvasHeight = 600

type Pos = (Int,Int)

handleKeyPress key stateRef = do
    state <- readIORef stateRef
    newState <- Game.keyPress key state
    writeIORef stateRef newState
    
gameLoop stateRef = do
    state <- readIORef stateRef
    newState <- Game.tick state
    State.printState newState
    writeIORef stateRef newState

main = do
    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False

    canvas <- drawingAreaNew
    canvas `on` sizeRequest $ return (Requisition canvasWidth canvasHeight)

    stateRef <- newIORef State.newState

    canvas `on` exposeEvent $ do
        drawWin <- eventWindow
        exposeRegion <- eventRegion
        liftIO $ do

        renderWithDrawable drawWin $ do
            region exposeRegion
            clip

            -- Draw the background
            save
            setSourceRGB 0.8 0.8 0.8
            paint
            restore

            -- Draw the tetrimino
            -- FIXME: currently just drawing a random piece in a fixed position
            drawPiece randomPiece (2,2)

        return True


    vbox <- vBoxNew False 0
    boxPackStartDefaults vbox canvas

    containerAdd window vbox
    widgetShowAll window
    
    window `on` keyPressEvent $ tryEvent $ do
        key <- eventKeyName
        liftIO $ handleKeyPress key stateRef
    
    --timeout value seems to not work?
    timeoutAdd (gameLoop stateRef >> return True) 1000
   
    mainGUI


drawPiece :: Piece -> Pos -> Render ()
drawPiece piece (x,y) = do
    save
    translate (fromIntegral x * blockSize) (fromIntegral y * blockSize)

    let coords = getCoords piece
    mapM drawBlock coords

    restore


drawBlock :: Pos -> Render ()
drawBlock (x,y) | trace ("drawBlock (" ++ show x ++ "," ++ show y ++ ")") False = undefined
drawBlock (x,y) | otherwise = do
--drawBlock (x,y) = do
    save
    translate (fromIntegral x * blockSize) (fromIntegral y * blockSize)

    -- Colour the box
    setSourceRGB 0 0 1 -- blue
    rectangle 0 0 blockSize blockSize
    fill

    -- Black border
    setSourceRGB 0 0 0 -- black
    setLineWidth 3
    rectangle (0 + 1.5) (0 + 1.5) (blockSize - 3) (blockSize - 3)
    stroke

    -- FIXME: draw shadows so the block looks raised

    restore
