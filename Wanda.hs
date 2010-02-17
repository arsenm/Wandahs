
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
  putStrLn $ if isJust colormap
               then "YES"
               else "No"
  maybe (return ()) (widgetSetColormap widget) colormap

--setAlpha window --TODO: also call setAlpha on alpha screen change

getMask :: Int -> Int -> IO Pixmap
getMask w h = do
  pb <- pixmapNew (Nothing :: Maybe DrawWindow) w h (Just 1)
-- ???
--  gcVals <- newGCValues
--  let newGcVal = gcVals { function = Nor }
  return pb



--  gc <- gcNew
--  gcVals <- newGCValues

--  let newGcVal = gcVals { clipMask = ??? }
-- 0,0 origin image?
--  drawPixbuf drawWindow gc pb 0 0 0 0 w h RgbDitherNormal 0 0

splitStrip :: Pixbuf -> Int -> IO (Array Int Pixbuf)
splitStrip orig n = do
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

-- (/!/) :: Ix i => Array i e -> i -> e
every = flip timeoutAdd

main = do
  initGUI


  image <- imageNew
  wandaStrip <- pixbufNewFromFile "/home/matt/src/wandahs/wanda.png"

  wandaAnimFrames <- splitStrip wandaStrip fishCount
  let wandaAnim = wandaAnimFrames ! 1
--  wandaAnim <- pixbufAnimationNewFromFile "/home/matt/Desktop/atom.gif"

--  wandaAnim <- pixbufAddAlpha wandaAnim (Just 0 0 0)
--  pixbufFill wandaAnim 0 0 0 0
  imageSetFromPixbuf image wandaAnim

  widgetSetDoubleBuffered image True

  screen <- widgetGetScreen image
  cmap <- screenGetRGBAColormap screen

  win <- windowNew -- Popup -- ??
  widgetSetAppPaintable win True -- ???
  onHide win mainQuit

  setAlpha win
  setAlpha image



{-
  box <- vBoxNew False 0
  btn <- buttonNewWithLabel "arst"
  boxPackStart box btn PackNatural 0
-}



--  windowSetRole win "Dialog"

  set win [ containerChild := image,
            windowTitle := "Wanda",
--            windowPosition := WinPosMouse,
            windowDecorated := False,
            windowTypeHint := WindowTypeHintDock, -- Dock
            windowDefaultHeight := 55,
            windowDefaultWidth := 90,
            windowAcceptFocus := True, -- False?
            windowResizable := True, -- False
            windowSkipTaskbarHint := True,
            windowSkipPagerHint := True,
            windowModal := True ]
          --windowKeepAbove := True,
          --windowOpacity := 0.5 ]
--  windowSetKeepAbove win True
--  windowSetHasFrame win False
  windowSetPosition win WinPosNone

  widgetShowAll win

  winDraw <- widgetGetDrawWindow win
  invisiCairo winDraw


  onExpose win (\_ -> invisiCairo winDraw >> return False)

  frameRef <- newIORef 1
  every 100 $ do (x,y) <- windowGetPosition win
                 windowMove win (x + 15) y
                 modifyIORef frameRef (nextFrame wandaAnimFrames)
                 next <- readIORef frameRef
                 imageSetFromPixbuf image (wandaAnimFrames ! next)
                 return True


--  drawWindowShapeCombineMask winDraw Nothing (-1) (-1)

--  drawableGetSize winDraw >>= print
--  gc <- gcNew winDraw
--drawPolygon winDraw gc True [(0,0),(30,30),(0,30),(30,0)]
--  drawPixbuf winDraw gc wandaAnim 0 0 0 0 (-1) (-1) RgbDitherNormal 0 0

  mainGUI

