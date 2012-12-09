import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)
 
main = do
   initGUI
   window <- windowNew
   window `on` deleteEvent $ liftIO mainQuit >> return False
   -- i.e., on window deleteEvent (liftIO mainQuit >> return False)
   widgetShowAll window
   
   window `on` keyPressEvent $ tryEvent $ do
    "w" <- eventKeyName
    liftIO $ putStrLn "w pressed! WEEE"
   
   --Gtk.onKeyPress window $ \ (Gtk.Key rel _ _ mods _ _ _ val name char) -> do
   -- keyEvent state rel mods val name char
   
   mainGUI
