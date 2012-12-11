module Main (main) where

--Lets keep all the ugly IO stufs in here.

import Test.QuickCheck
import Data.IORef
import Data.Maybe(fromJust)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
import Game as Game
import Piece
import State as State
import Debug.Trace
import System.Random

blockSize = 30

main = do
    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False

    canvas <- drawingAreaNew
    canvas `on` sizeRequest $ return (Requisition (blockSize*10) (blockSize*20))

    seed <- newStdGen
    piecesRef <- newIORef $ randomPieces seed
    pieces <- readIORef piecesRef

    stateRef <- newIORef $ State.newState (Just $ head pieces)
    writeIORef piecesRef $ tail pieces

    let handleKeyPress key = do
        state <- readIORef stateRef
        newState <- keyPress key state
        widgetQueueDraw canvas -- FIXME: should invalidate only the part of the canvas that has changed
        writeIORef stateRef newState

    let gameLoop time = do
        state <- readIORef stateRef
        newState <- tick state

        case activePiece newState of
            Nothing -> do
                pieces <- readIORef piecesRef
                writeIORef stateRef $ State.setPiece newState (head pieces) (4,0)
                writeIORef piecesRef $ tail pieces
                timeoutAdd (gameLoop (time-10) >> return False) time
                return ()
            _           -> do
                writeIORef stateRef newState
                timeoutAdd (gameLoop time >> return False) time
                return ()

        widgetQueueDraw canvas -- FIXME: should invalidate only the part of the canvas that has changed
        
    canvas `on` exposeEvent $ do
        drawWin <- eventWindow
        exposeRegion <- eventRegion
        liftIO $ do
        state <- readIORef stateRef

        renderWithDrawable drawWin $ do
            region exposeRegion
            clip

            -- Draw the background
            save
            setSourceRGB 0.8 0.8 0.8
            paint
            restore

            -- Draw the board
            drawGrid (grid state)

            -- Draw the tetrimino
            -- FIXME: currently just drawing a random piece in a fixed position
            let maybePiece = getPiece state
            case maybePiece of
                Just piece  -> drawPiece piece (position state)
                _           -> return ()

        return True


    vbox <- vBoxNew False 0
    boxPackStartDefaults vbox canvas

    containerAdd window vbox
    widgetShowAll window
    
    window `on` keyPressEvent $ tryEvent $ do
        key <- eventKeyName
        liftIO $ handleKeyPress key
    
    timeoutAdd (gameLoop 1000 >> return False) 1000
   
    mainGUI


tick :: State -> IO State
tick state = do
    tick' (State.grid state) (fromJust $ State.activePiece state) (State.position state) state

tick' :: Grid -> Piece -> Pos -> State -> IO State
tick' board piece (x,y) state = do
    if State.canPlace board (piecee piece) (x,(y+1))
        then return $ State.move state 0 1
        else return $ pieceDone board (points state) piece (x,y)

pieceDone :: Grid -> Int -> Piece -> Pos -> State
pieceDone g points ap (x,y)
    | y == 0    = error "End of game" -- End of the game, crash for now
    | otherwise =
        createState (makeGrid newnewg) (points+newpoints) Nothing (4,0)
        where
            (newpoints, newnewg) = rowReduce (rows newg)
            newg = addPieceToGrid ap g (x,y)


keyPress :: String -> State -> IO State
keyPress key state | trace ("keyPress (" ++ show key ++ ", ...)") False = undefined
                   | otherwise = do
    case key of
        "Up"    -> return $ tryRotate state
        "Down"  -> return $ tryMove state 0 1
        "Left"  -> return $ tryMove state (-1) 0
        "Right" -> return $ tryMove state 1 0
        "space" -> return $ dropPiece state
        _       -> return state


drawGrid :: Grid -> Render ()
drawGrid grid = do
    save

    let coords = getGridCoords grid
    mapM drawBlock coords

    restore


drawPiece :: Piece -> Pos -> Render ()
drawPiece piece (x,y) = do
    save
    translate (fromIntegral x * fromIntegral blockSize) (fromIntegral y * fromIntegral blockSize)

    let coords = getCoords piece
    mapM drawBlock coords

    restore


drawBlock :: Pos -> Render ()
drawBlock (x,y) | trace ("drawBlock (" ++ show x ++ "," ++ show y ++ ")") False = undefined
drawBlock (x,y) | otherwise = do
--drawBlock (x,y) = do
    save
    translate (fromIntegral x * fromIntegral blockSize) (fromIntegral y * fromIntegral blockSize)

    -- Colour the box
    setSourceRGB 0 0 1 -- blue
    rectangle 0 0 (fromIntegral blockSize) (fromIntegral blockSize)
    fill

    -- Black border
    setSourceRGB 0 0 0 -- black
    setLineWidth 3
    rectangle (0 + 1.5) (0 + 1.5) (fromIntegral blockSize - 3) (fromIntegral blockSize - 3)
    stroke

    -- FIXME: draw shadows so the block looks raised

    restore
