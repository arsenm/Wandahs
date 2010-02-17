
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C

import Graphics.UI.Gtk


import Graphics.UI.Gtk.Gdk.Screen
import System.Glib.Attributes


import Control.Monad
import Data.Maybe
import Data.Array
import Data.IORef


import Graphics.UI.Gtk.Types
--import Graphics.UI.Gtk.Windows.Window
--import System.Glib.GObject

--import WandaLib


--GdkScreen *screen = gtk_widget_get_screen(widget);
--GdkColormap *colormap = gdk_screen_get_rgba_colormap(screen);
--    /* Now we have a colormap appropriate for the screen, use it */
--    gtk_widget_set_colormap(widget, colormap);


setAlpha widget = do
  screen <- widgetGetScreen widget
  colormap <- screenGetRGBAColormap screen
  maybe (return ()) (widgetSetColormap widget) colormap

--setAlpha window --TODO: also call setAlpha on alpha screen change


getMask :: Int -> Int -> IO Pixmap
getMask w h = do
  pb <- pixmapNew (Nothing :: Maybe DrawWindow) w h (Just 1)
  return pb


splitStrip :: Int -> Pixbuf -> IO (Array Int Pixbuf)
splitStrip n orig = do
  w <- pixbufGetWidth orig
  h <- pixbufGetHeight orig
  let iw = w `div` n
  let ps = take n $ iterate (+iw) 0
  let split x = pixbufNewSubpixbuf orig x 0 iw h
  return . listArray (1,n) =<< mapM split ps


invisiCairo dw = renderWithDrawable dw drawTransparent
  where drawTransparent = do
          setSourceRGBA 1.0 1.0 1.0 0.0
          setOperator OperatorSource
          paint


fishCount = 8

--TODO: Backwards
nextFrame :: Array Int Pixbuf -> Int -> Int
nextFrame arr i = let (l,u) = bounds arr
                      next = i + 1
                  in if next > u
                       then l
                       else next

every = flip timeoutAdd

main = do
  initGUI

  image <- imageNew
  widgetSetDoubleBuffered image True
  setAlpha image

  -- get the stip of fish pictures and split it into an array of frames
  wandaFrames <- splitStrip fishCount =<< pixbufNewFromFile "/home/matt/src/wandahs/wanda.png"
  let wandaAnim = wandaFrames ! 1
  imageSetFromPixbuf image wandaAnim

 -- Setup the window. Needs to be drawable for transparency to work.
  win <- windowNew
  widgetSetAppPaintable win True
  onHide win mainQuit
  setAlpha win

  set win [ containerChild := image,
            windowTitle := "Wanda",
            windowDecorated := False,
            windowTypeHint := WindowTypeHintDock, -- Dock
            windowDefaultHeight := 55,
            windowDefaultWidth := 90,
            windowAcceptFocus := True, -- False?
            windowResizable := False,
            windowSkipTaskbarHint := True,
            windowSkipPagerHint := True,
            windowModal := True ]

  windowSetPosition win WinPosNone
  windowSetKeepAbove win True
  windowSetRole win "Desktop Fish"
--windowSetHasFrame win False

  widgetShowAll win

  winDraw <- widgetGetDrawWindow win

  -- The transparency needs to be redone as the window is redrawn
  onExpose win (\_ -> invisiCairo winDraw >> return False)

  imgDraw <- widgetGetDrawWindow image

  frameRef <- newIORef 1
  every 100 $ do (x,y) <- windowGetPosition win
                 windowMove win (x + 3) y
                 modifyIORef frameRef (nextFrame wandaFrames)
                 next <- readIORef frameRef
                 imageSetFromPixbuf image (wandaFrames ! next)

                 (iw, ih) <- drawableGetSize imgDraw

                 -- TODO: I can't get the clip to stop clickthrough
                 -- without a function that needs to be bound in
                 -- gtk2hs

                 --pm <- pixmapNew (Just imgDraw) iw ih (Just 1)
                 --pm <- pixmapNew (Nothing::Maybe DrawWindow) iw ih (Just 1)
                 --widgetShapeCombineMask      win (Just pm) 0 0
                 --widgetInputShapeCombineMask win (Just pm) 0 0

                 return True

  mainGUI

