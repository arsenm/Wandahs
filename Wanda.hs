{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS_GHC -W -O2 -funbox-strict-fields #-}
{-# CFILES wanda_image.c #-}

import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Screen

import System.Glib.Attributes hiding (get)

--import Control.Monad
--import Control.Arrow
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Data.Array
import Data.IORef

import Foreign.Ptr

import Random

import Debug.Trace

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
fishHeight = 55
fishWidth = 90
defaultSpeed = 5


frame = snd . curFrame
frameIdx = fst . curFrame

type Pos = (Int, Int)
type Vec = (Int, Int)

data FishState = FishState { pos :: !Pos,
                             dest :: !Pos,
                             curFrame :: !(Int, Pixbuf),
                             frameN :: !Int,
                             backwards :: !Bool,
                             speed :: !Int,
                             rndGen :: StdGen,
                             screenSize :: !(Int,Int),
                             frames :: Array Int (Pixbuf, Pixbuf)
                           }
                 --running away?
                 -- size?
                 -- maybe not?


dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = round . sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2


updateFishState = execState fishTick

updatePos newPos = modify (\s -> s { pos = newPos })

fishTick :: State FishState ()
fishTick = do
  spd     <- gets speed
  p@(x,y) <- gets pos
  d       <- gets dest

  let (dx,dy) = vec spd p d
  trace ("dx: " ++ show dx ++ "  dy " ++ show dy) (return ())
  trace ("p: " ++ show p ++ "  d: " ++ show d) (return ())

 -- avoid getting stuck, and get close enough
  when ((dx == 0 && dy == 0) || (dist p d <= spd) ) newDest
  updatePos (x + dx, y + dy)
  updateFrame


-- sqrt (dx^2 + (k * dx) ^2) = s^2
-- k = ratio of distance to travel
-- result is a vector with length s
-- with proportions depending on how far in what direction to do

-- | Approximate a dx/dy vector of ~length d from the first point to
-- the second with a ratio depending on the distance to be traveled.
vec :: Int -> Pos -> Pos -> Vec
vec d (x1,y1) (x2,y2) = let a = fromIntegral (x2 - x1)
                            b = fromIntegral (y2 - y1)
                            k = abs (b / a)
                            s = fromIntegral d

                            dx = sqrt (s^2 / (1 + k^2))
                            dy = k * dx

               {-
                            ddx = if a > 0
                                    then ceiling dx
                                    else floor (negate dx)
               -}
                            ddx = if a > 0
                                    then dx
                                    else negate dx
                            ddy = if b > 0
                                     then dy
                                     else negate dy

                        in (floor ddx, floor ddy)
--TODO: floor or round? ceiling?
--FIXME: Use floor with negative numbers?
--FIXME: dx, dy configurable speed maybe
--Also prevent dx dy = 0 from ever happening.
{-
assert :: String -> Bool -> a -> a
assert s c v = if c
                 then error ("Assertion failed: " ++ s)
                 else v


printIf :: String -> Bool -> a -> a
printIf s c v = if c
                  then trace ("Cond: " ++ s) v
                  else v
-}

-- updates to a new destination and randomgen
newDest :: State FishState ()
newDest = do
  (sx, sy) <- gets screenSize
  gen      <- gets rndGen

--CHECKME: Screen from 0 or 1?
--TODO: lower and upper some percent beyond limit
  let (x, gen')  = randomR ((-10), sx + 10) gen
  let (y, gen'') = randomR ((-10), sy + 10) gen'
  modify (\s -> s { rndGen = gen'',
                    dest = (x,y)
                  } )
  setBackwards


--TODO: Reorganize to have a separate backwards array
setBackwards :: State FishState ()
setBackwards = do
  (cx, _) <- gets pos
  (dx, _) <- gets dest
  let bw = dx < cx
  let acc = if bw then fst else snd

  arr <- gets frames
  i   <- gets frameIdx

  modify (\s -> s { curFrame = (i, acc (arr ! i)),
                    backwards = bw
                  })


fishIO :: Image -> Window -> FishState -> IO ()
fishIO img win fs = do
  let move = uncurry (windowMove win)
  move (pos fs)
  imageSetFromPixbuf img (frame fs)



--CHECKME: Does it actually matter which way you move through the
--frames with respect to travel direction

-- I may be retarded but mod breaks with going backwards
wrap arr x | x > u     = l
           | x < l     = u
           | otherwise = x
             where (l,u) = bounds arr


-- | Move the current frame to the next depending on the direction of
-- travel. Backwards means fish moving right to left. Forwards is left
-- to right.
updateFrame :: State FishState ()
updateFrame = do
  arr <- gets frames
  bw  <- gets backwards
  idx <- gets frameIdx
  let (acc, inc) = if bw
                     then (fst, (-))
                     else (snd, (+))
      n = arr `wrap` (idx `inc` 1)
      pb = acc (arr ! n)
  modify (\s -> s { curFrame = (n,pb) })


randomIOPos :: IO Pos
randomIOPos = liftA2 (,) randomIO randomIO

every = flip timeoutAdd

 --pixbufNewFromFile "/home/matt/src/wandahs/wanda.png"

getScreenSize :: Screen -> IO (Int, Int)
getScreenSize scr = liftA2 (,) (screenGetWidth scr) (screenGetHeight scr)

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
            windowGravity := GravityCenter,
            windowDefaultHeight := fishHeight,
            windowDefaultWidth := fishWidth,
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

--TODO: Update screen size changed signal



  winPos <- windowGetPosition win
  newGen <- getStdGen
  scrSize <- getScreenSize =<< widgetGetScreen win
--iniDest <- randomIOPos
  let iniDest = (100, 100)
  let iniFishState = FishState { pos = winPos,
                                 curFrame = (1, iniFrame),
                                 frameN = fishCount,
                                 backwards = False,
                                 speed = defaultSpeed,
                                 frames = wandaFrames,
                                 dest = iniDest,
                                 screenSize = scrSize,
                                 rndGen = newGen
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

