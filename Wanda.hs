{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS_GHC -W -O2 -funbox-strict-fields #-}
{-# CFILES wanda_image.c #-}

import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Screen

import System.Glib.Attributes

--import Control.Monad
--import Control.Monad.State
--import Control.Applicative
import Data.Maybe
import Data.Array
import Data.IORef

import Foreign.Ptr

foreign import ccall "wanda_image.h &wandaimage"
  wandaImage :: Ptr InlineImage


setAlpha widget = do
  screen <- widgetGetScreen widget
  colormap <- screenGetRGBAColormap screen
  maybe (return ()) (widgetSetColormap widget) colormap

--setAlpha window --TODO: also call setAlpha on alpha screen change


{-
getMask :: Int -> Int -> IO Pixmap
getMask w h = do
  pb <- pixmapNew (Nothing :: Maybe DrawWindow) w h (Just 1)
  return pb
-}

-- pair of pixbufs. Left points left, right fish goes right
splitStrip :: Int -> Pixbuf -> IO (Array Int (Pixbuf, Pixbuf))
splitStrip n orig = do
  w <- pixbufGetWidth orig
  h <- pixbufGetHeight orig
  let iw = w `div` n
  let ps = take n $ iterate (+iw) 0
  let split x = do l <- pixbufNewSubpixbuf orig x 0 iw h
                   r <- pixbufFlipHorizontally l
                   return (l,r)
  return . listArray (1,n) =<< mapM split ps


invisiCairo dw = renderWithDrawable dw drawTransparent
  where drawTransparent = do
          setSourceRGBA 1.0 1.0 1.0 0.0
          setOperator OperatorSource
          paint


fishCount = 8

frame = snd . curFrame

type Pos = (Int, Int)

data FishState = FishState { pos :: !Pos,
                             curFrame :: !(Int, Pixbuf),
                             frameN :: !Int,
                             backwards :: !Bool,
                             speed :: !Int,
                             frames :: Array Int (Pixbuf, Pixbuf)
                           }
                 --running away?
                 -- speed?
                 -- size?
                 -- maybe not?

updateFishState :: FishState -> FishState
updateFishState st = st { curFrame = nextFrame st,
                          pos = newPos
                        }
  where (x,y) = pos st
        spd = speed st
        newPos = (x + spd, y + spd)

fishIO :: Image -> Window -> FishState -> IO ()
fishIO img win fs = do
  let move = uncurry (windowMove win)
  move (pos fs)
  imageSetFromPixbuf img (frame fs)



--CHECKME: Does it actually matter which way you move through the
--frames with respect to travel direction

wrap arr x | x > u     = l
           | x < l     = u
           | otherwise = x
             where (l,u) = bounds arr


-- Get next frame and index depending on stuff
nextFrame :: FishState -> (Int, Pixbuf)
nextFrame st = let arr = frames st
                   i = fst (curFrame st)
                   (get, inc) = if backwards st
                                  then (fst, (-))
                                  else (snd, (+))
                   next = arr `wrap` (i `inc` speed st)
               in (next, get (arr ! next))

every = flip timeoutAdd

 --pixbufNewFromFile "/home/matt/src/wandahs/wanda.png"

fishFrames :: IO (Array Int (Pixbuf, Pixbuf))
fishFrames = splitStrip fishCount =<< pixbufNewFromInline wandaImage

main = do
  initGUI

  img <- imageNew
  widgetSetDoubleBuffered img True

  -- get the stip of fish pictures and split it into an array of frames
  wandaFrames <- fishFrames

 -- Setup the window. Needs to be drawable for transparency to work.
  win <- windowNew
  widgetSetAppPaintable win True
  onHide win mainQuit
  setAlpha win

  set win [ containerChild := img,
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

-- FIXME: Some kind of flicker on start. Maybe start offscreen?

  -- The transparency needs to be redone as the window is redrawn
  winDraw <- widgetGetDrawWindow win
  onExpose win (\_ -> invisiCairo winDraw >> return False)

  let iniFrame = snd (wandaFrames ! 1)

  winPos <- windowGetPosition win
  let iniFishState = FishState { pos = winPos,
                                 curFrame = (1, iniFrame),
                                 frameN = fishCount,
                                 backwards = False,
                                 speed = 3,
                                 frames = wandaFrames
                               }
  -- set initial image
  imageSetFromPixbuf img iniFrame

--imgDraw <- widgetGetDrawWindow img
  fishRef <- newIORef iniFishState


-- Can I assume the position is what I set last time?  If the window
-- manager isn't respecting move, nothing will work anyways
  every 100 $ do modifyIORef fishRef updateFishState
                 fishIO img win =<< readIORef fishRef
                 return True

  mainGUI


-- TODO: I can't get the clip to stop clickthrough
-- without a function that needs to be bound in
-- gtk2hs

--pm <- pixmapNew (Just imgDraw) iw ih (Just 1)
--pm <- pixmapNew (Nothing::Maybe DrawWindow) iw ih (Just 1)
--widgetShapeCombineMask      win (Just pm) 0 0
--widgetInputShapeCombineMask win (Just pm) 0 0

