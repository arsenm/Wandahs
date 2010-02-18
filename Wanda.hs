{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS_GHC -W -O2 -funbox-strict-fields #-}
{-# CFILES wanda_image.c #-}

import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Screen

import System.Glib.Attributes hiding (get)

import Control.Arrow
import Control.Monad.State

import Control.Applicative
import Data.Maybe
import Data.Array
import Data.IORef

import Foreign.Ptr

import Random


foreign import ccall "wanda_image.h &wandaimage"
  wandaImage :: Ptr InlineImage

type Pos = (Int, Int)
type Vec = (Int, Int)

-- | Track the state of the fish
data FishState = FishState { pos :: !Pos,
                             dest :: !Pos,
                             curFrame :: !Int,
                             frameN :: !Int,
                             backwards :: !Bool,
                             speed :: !Int,
                             rndGen :: StdGen,
                             screenSize :: !(Int,Int),
                             frames :: Array Int Pixbuf,
                             backFrames :: Array Int Pixbuf
                           }
                 --running away?
                 -- size?
                 -- maybe not?

-- | Set a widget to use an RGBA colormap
setAlpha :: (WidgetClass widget) => widget -> IO ()
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


-- | Split the original image into a pair of forwards and backwards
-- facing Pixbufs arrays. Left array has left facing fishes
-- (backwards). Right array has right going fishes.
splitStrip :: Int -> Pixbuf -> IO (Array Int Pixbuf, Array Int Pixbuf)
splitStrip n orig = do
  w <- pixbufGetWidth orig
  h <- pixbufGetHeight orig
  let iw = w `div` n
  let ps = take n $ iterate (+iw) 0
  let split x = do l <- pixbufNewSubpixbuf orig x 0 iw h
                   r <- pixbufFlipHorizontally l
                   return (l,r)
  let arr = listArray (0, n-1)
      both =  arr *** (arr . reverse)
  both . unzip <$> mapM split ps


--both :: (a -> b) -> (a, a) -> (b, b)
--both f = f *** f

-- | Draw the background transparently.
invisiCairo :: (DrawableClass dw) => dw -> IO ()
invisiCairo dw = renderWithDrawable dw drawTransparent
  where drawTransparent = do
          setSourceRGBA 1.0 1.0 1.0 0.0
          setOperator OperatorSource
          paint


fishCount = 8
fishHeight = 55
fishWidth = 90
defaultSpeed = 5

-- Unfortunately the wrong kind of scale.
-- Fish puns for great justice.
fishScale = 0.5


-- | Calculate the Cartesian distance between two points, approximated to closest integer
dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = round . sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

updateFishState = execState fishTick

-- | Updates a fish state with a new position
updatePos newPos = modify (\s -> s { pos = newPos })

-- | Main fish state change function
fishTick :: State FishState ()
fishTick = do
  spd     <- gets speed
  p@(x,y) <- gets pos
  d       <- gets dest

  let (dx,dy) = vec spd p d

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

                        in (round ddx, round ddy)
--TODO: floor or round? ceiling?
--TODO: Use floor with negative numbers?
--TODO: dx, dy configurable speed maybe

-- updates to a new destination and randomgen
newDest :: State FishState ()
newDest = do
  (sx, sy) <- gets screenSize
  gen      <- gets rndGen

  --TODO: Maybe some kind of biasing to get longer, smoother
  --paths. Sometimes can get a close by new destination and a weird
  --jerk could happen

  -- limits are (screen x +- 10%, screen y +- 10%)
  let (tx,ty) = (sx `div` 10, sy `div` 10)
  let xBnds   = (negate tx, sx + tx)
  let yBnds   = (negate ty, sy + ty)

  let (x, gen')  = randomR xBnds gen
  let (y, gen'') = randomR yBnds gen'
  modify (\s -> s { rndGen = gen'',
                    dest = (x,y)
                  } )
  setBackwards


--TODO: Reorganize to have a separate backwards array
setBackwards :: State FishState ()
setBackwards = do
  (cx, _) <- gets pos
  (dx, _) <- gets dest
  i <- gets curFrame
  n <- gets frameN

  oldBw <- gets backwards

  let bw = dx < cx

  let i' = if oldBw /= bw   -- since the images are reversed, on direction change flip index
           then n - i - 1   -- otherwise there would be a weird jump
           else i           -- n - i - 1 is same image in other direction.

  modify (\s -> s { backwards = bw,
                    curFrame  = i' })

move :: Window -> Pos -> IO ()
move = uncurry . windowMove

-- | Read the frame from the appropriate array depending on travel
-- direction
getFrame :: FishState -> Pixbuf
getFrame s = let arr = if backwards s
                         then backFrames s
                         else frames s
             in arr ! curFrame s

-- | Perform the IO needed for a fish update, i.e. move the window and
-- update the image
fishIO :: Image -> Window -> FishState -> IO ()
fishIO img win fs = do
  move win (pos fs)
  imageSetFromPixbuf img (getFrame fs)



--CHECKME: Does it actually matter which way you move through the
--frames with respect to travel direction


-- | Move the current frame to the next depending on the direction of
-- travel. Backwards means fish moving right to left. Forwards is left
-- to right.
updateFrame :: State FishState ()
updateFrame = do
  i <- gets curFrame
  n <- gets frameN
  let i' = (i + 1) `mod` n
  modify (\s -> s { curFrame = i' })


-- | Simply select a random position on the screen
randomIOPos :: Screen -> IO Pos
randomIOPos scr = getScreenSize scr >>= \bnds ->
                    liftA2 (,) (randomRIO bnds) (randomRIO bnds)


-- | Same as randomIOPos, except limited to a small offscreen part
randomIOStart :: Screen -> IO Pos
randomIOStart scr = do
  (sx, sy) <- getScreenSize scr
  let tx = sx `div` 10
  let xBnds = (negate tx, 0)
  let yBnds = (0, sy)

  liftA2 (,) (randomRIO xBnds) (randomRIO yBnds)
--TODO: Probability of starting on right, but need to also set backwards
--  let leftP = randomIO



every = flip timeoutAdd

-- | Get the screen size as a pair
getScreenSize :: Screen -> IO (Int, Int)
getScreenSize scr = liftA2 (,) (screenGetWidth scr) (screenGetHeight scr)


-- | Split the image up into the individual fish frames
fishFrames :: IO (Array Int Pixbuf, Array Int Pixbuf)
fishFrames = splitStrip fishCount =<< pixbufNewFromInline wandaImage


-- | Takes the (forward frames, backward frames) and creates a new wanda window
createWanda :: (Array Int Pixbuf, Array Int Pixbuf) -> IO Window
createWanda (bckFrames, fwdFrames) = do
  img <- imageNew
  widgetSetDoubleBuffered img True

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

--TODO: Update screen size changed signal

  scr <- widgetGetScreen win

  newGen  <- newStdGen
  scrSize <- getScreenSize scr
  iniPos  <- randomIOStart scr  -- random offscreen location to start from
  iniDest <- randomIOPos   scr  -- where to go from there

  --TODO: be able to start from right / backwards

  move win iniPos

  let iniFishState = FishState { pos = iniPos,
                                 curFrame = 1,
                                 frameN = fishCount,
                                 backwards = False,
                                 speed = defaultSpeed,
                                 dest = iniDest,
                                 screenSize = scrSize,
                                 rndGen = newGen,
                                 frames = fwdFrames,
                                 backFrames = bckFrames
                               }
  -- set initial image
  imageSetFromPixbuf img (fwdFrames ! 1)

--imgDraw <- widgetGetDrawWindow img
  fishRef <- newIORef iniFishState


-- Can I assume the position is what I set last time?  If the window
-- manager isn't respecting move, nothing will work anyways
  every 100 $ do modifyIORef fishRef updateFishState
                 fishIO img win =<< readIORef fishRef
                 return True

  return win

main :: IO ()
main = do
  initGUI
  fr <- fishFrames

--FIXME: Still observing some moving backwards with forwards image in wanda parade

  createWanda fr
--  replicateM_ 10 (createWanda fr)

  mainGUI


-- TODO: I can't get the clip to stop clickthrough
-- without a function that needs to be bound in
-- gtk2hs

--pm <- pixmapNew (Just imgDraw) iw ih (Just 1)
--pm <- pixmapNew (Nothing::Maybe DrawWindow) iw ih (Just 1)
--widgetShapeCombineMask      win (Just pm) 0 0
--widgetInputShapeCombineMask win (Just pm) 0 0

