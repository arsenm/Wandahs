{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# OPTIONS_GHC -W -funbox-strict-fields #-}
{-# CFILES wanda_image.c #-}

import Graphics.Rendering.Cairo

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import qualified Data.ByteString.Lazy as B

import Control.Applicative
import Control.Arrow
import Control.Monad.State

import Data.Array
import Data.Binary.Get
import Data.IORef

import Foreign.Ptr

import System.IO
import System.Random.Mersenne.Pure64


foreign import ccall "wanda_image.h &wandaimage"
  wandaImage :: Ptr InlineImage

type Pos = (Int, Int)
type Vec = (Int, Int)
type Fish = Window
type FishFrame = (Pixbuf, Bitmap)


--TODO: Better seeding not based off clock. With clock, seem to get
-- wanda schools moving in same general way?

-- | Track the state of the fish
data FishState = FishState { dest :: !Pos,
                             curFrame :: !Int,
                             frameN :: !Int,
                             backwards :: !Bool,
                             speed :: !Int,
                             rndGen :: PureMT,
                             screenSize :: !(Int,Int),
                             frames :: Array Int FishFrame,
                             backFrames :: Array Int FishFrame
                           }
                 -- running away?
                 -- size?
                 -- maybe not?
                 -- Speech bubble?

instance Show FishState where
  show s = unlines ["FishState {\n",
                    "dest = "            ++ show (dest s),
                    "curFrame = "        ++ show (curFrame s),
                    "backwards = "       ++ show (backwards s)]



fishCount = 8
fishHeight = 55
fishWidth = 90
defaultSpeed = 5
fishTimeout = 100

-- Unfortunately the wrong kind of scale.
-- Fish puns for great justice.
--fishScale = Just 0.5
fishScale = Nothing


-- | Set a widget to use an RGBA colormap
setAlpha :: (WidgetClass widget) => widget -> IO ()
setAlpha widget = do
  screen <- widgetGetScreen widget
  colormap <- screenGetRGBAColormap screen
  maybe (return ()) (widgetSetColormap widget) colormap

--setAlpha window --TODO: also call setAlpha on alpha screen change



-- | Draw the background transparently.
transparentBG :: (DrawableClass dw) => dw -> IO ()
transparentBG dw = renderWithDrawable dw drawTransparent
  where drawTransparent = do
          setSourceRGBA 1.0 1.0 1.0 0.0
          setOperator OperatorSource
          paint


-- | Split the original image into a pair of forwards and backwards
-- facing Pixbufs arrays, scaling the image by a factor. Left array
-- has left facing fishes (backwards). Right array has right going
-- fishes.
splitStrip :: Maybe Double -> Int -> Pixbuf -> IO (Array Int FishFrame, Array Int FishFrame)
splitStrip scale n img = do
  w <- pixbufGetWidth img
  h <- pixbufGetHeight img

 -- optionally scale the image. I don't just scale by a factor of 1
 -- for Nothing since I figure that could screw things up a bit.
  (img',w',h') <- case scale of
                    Nothing -> return (img, w, h)
                    Just k  -> do let ws = floor (k * fromIntegral w)
                                      hs = floor (k * fromIntegral h)
                                  i <- pixbufScaleSimple img ws hs InterpHyper
                                  return (i, ws, hs)

  let iw = w' `div` n
  let ps = take n $ iterate (+iw) 0
  let split x = do l  <- pixbufNewSubpixbuf img' x 0 iw h'
                   lm <- getMask l
                   r  <- pixbufFlipHorizontally l
                   rm <- getMask r
                   return ((l,lm), (r, rm))
  let arr = listArray (0, n-1)
      both =  arr *** (arr . reverse)
  both . unzip <$> mapM split ps


-- | Change direction. This prevents a possible discontinuity in the
-- animation
swapDirection :: State FishState ()
swapDirection = do
  i  <- gets curFrame
  n  <- gets frameN
  bw <- gets backwards

  let i' = n - i - 1  -- since the images are reversed, on direction change flip index
                      -- otherwise there would be a weird jump
                      -- n - i - 1 is same image in other direction.

  modify (\s -> s { backwards = not bw,
                    curFrame  = i' })

-- Fish tick? Fish stick?
-- | Main fish state change function
fishTick :: Pos -> State FishState Pos
fishTick p@(x,y) = do
  spd     <- gets speed
  d@(t,_) <- gets dest

  let (dx,dy) = vec spd p d
      x' = x + dx
      p' = (x', y + dy)

  -- Not on same side after moving, so turned around
  when ( (t < x) /= (t < x') ) swapDirection

  -- Avoid getting stuck, and get close enough
  when ((dx == 0 && dy == 0) || dist p' d <= spd) (newDest p')
  updateFrame
  return p'

-- | Move the current frame to the next depending on the direction of
-- travel. Backwards means fish moving right to left. Forwards is left
-- to right.
updateFrame :: State FishState ()
updateFrame = do
  i <- gets curFrame
  n <- gets frameN
  let i' = (i + 1) `mod` n
  modify (\s -> s { curFrame = i' })


-- | Pick a new destination and Randomgen
newDest :: Pos -> State FishState ()
newDest (x,_) = do
  (sx, sy) <- gets screenSize
  gen      <- gets rndGen
  oldBw    <- gets backwards
  i        <- gets curFrame
  n        <- gets frameN

  --TODO: Maybe some kind of biasing to get longer, smoother
  --paths. Sometimes can get a close by new destination and a weird
  --jerk could happen

  -- limits are (screen x +- 10%, screen y +- 10%)
  let (tx,ty)     = (sx `div` 10, sy `div` 10)
      xBnds       = (negate tx, sx + tx)
      yBnds       = (negate ty, sy + ty)
      (loc, gen') = randomPair gen xBnds yBnds

      bw = fst loc < x
      i' = if oldBw == bw
             then i
             else n - i - 1

  modify (\s -> s { rndGen = gen',
                    dest = loc,
                    backwards = bw,
                    curFrame = i'
                  } )


-- | Calculate the Cartesian distance between two points, approximated
-- to closest integer
dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = round . sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2


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

                            ddx = if a > 0
                                    then dx
                                    else negate dx
                            ddy = if b > 0
                                     then dy
                                     else negate dy

                        in (round ddx, round ddy)

--TODO: floor or round? ceiling? negative numbers?
--TODO: dx, dy configurable speed maybe



--TODO: Exceptions

newPureMTSysSeed :: IO PureMT
newPureMTSysSeed = do
  h <- openBinaryFile "/dev/urandom" ReadMode
  bs <- B.hGet h 8
  let x = runGet getWord64le bs
  hClose h
  return (pureMT x)


randomIntR :: (Int, Int) -> PureMT -> (Int, PureMT)
randomIntR (l,u) s = let n = u - l + 1
                         (v, s') = randomInt s
                     in (l + v `mod` n, s')

--CHECKME: Does it actually matter which way you move through the
--frames with respect to travel direction

-- | Read the frame from the appropriate array depending on travel
-- direction
getFrame :: FishState -> FishFrame
getFrame s = let arr = if backwards s
                         then backFrames s
                         else frames s
             in arr ! curFrame s


move :: Fish -> Pos -> IO ()
move = uncurry . windowMove


-- | Perform the IO needed for a fish update, i.e. move the window and
-- update the image
fishIO :: Image -> Fish -> IORef FishState -> IO Bool
fishIO img win ref = do
  r  <- windowGetPosition win

  (r', fs') <- runState (fishTick r) <$> readIORef ref

  move win r'

  let (fr,msk) = getFrame fs'

  imageSetFromPixbuf img fr

--TODO: Do I need to remove the old mask?
--It appears I do, at least for the input shape. I get gdk CRITICAL if I don't
-- set update clickthrough
--  widgetShapeCombineMask win Nothing 0 0
--  widgetShapeCombineMask win (Just msk) 0 0

--widgetInputShapeCombineMask win Nothing    0 0
--widgetInputShapeCombineMask win (Just msk) 0 0

  writeIORef ref fs'
  return True


-- | Simply select a random position on the screen, limited to the
-- screen area.
randomPos :: PureMT -> (Int, Int) -> (Pos, PureMT)
randomPos g (sx, sy) = randomPair g (0, sx) (0, sy)


--TODO: Probability of starting on right, but need to also set backwards

-- | Takes the bounds of the screen and produces a position off screen
-- to start from
randomStartPos :: PureMT -> (Int, Int) -> (Pos, PureMT)
randomStartPos g (sx, sy) = let tx = sx `div` 10
                                xBnds = (negate tx, 0)
                                yBnds = (0, sy)
                            in randomPair g xBnds yBnds


-- | Pass two sets of bounds for the first and second
randomPair :: PureMT -> (Int, Int) -> (Int, Int) -> (Pos, PureMT)
randomPair g xBnds yBnds = let (x, g')  = randomIntR xBnds g
                               (y, g'') = randomIntR yBnds g'
                           in ((x,y), g'')

every = flip timeoutAdd

-- | Get the screen size as a pair
getScreenSize :: Screen -> IO (Int, Int)
getScreenSize scr = liftA2 (,) (screenGetWidth scr) (screenGetHeight scr)


-- | Split the image up into the individual fish frames
fishFrames :: Maybe Double -> IO (Array Int FishFrame, Array Int FishFrame)
fishFrames scale = splitStrip scale fishCount =<< pixbufNewFromInline wandaImage


getMask :: Pixbuf -> IO Bitmap
getMask pb = do
  w <- pixbufGetWidth  pb
  h <- pixbufGetHeight pb

  bm <- pixmapNew (Nothing::Maybe DrawWindow) w h (Just 1)
  pixbufRenderThresholdAlpha pb bm 0 0 0 0 (-1) (-1) 1
  return bm



-- | Takes the (forward frames, backward frames) and creates a new wanda window
createWanda :: (Array Int FishFrame, Array Int FishFrame) -> IO Fish
createWanda (bckFrames, fwdFrames) = do
  img <- imageNew
  widgetSetDoubleBuffered img True

 -- Setup the window. Needs to be drawable for transparency to work.
  win <- windowNew
  set win [ containerChild := img,
            windowTitle := "Wanda",
            windowDecorated := False,
            windowTypeHint := WindowTypeHintDock, -- Dock
            windowGravity := GravityStatic,     -- Importantish.
            windowDefaultHeight := fishHeight,
            windowDefaultWidth := fishWidth,
            windowAcceptFocus := True, -- False?
            windowResizable := False,
            windowSkipTaskbarHint := True,
            windowSkipPagerHint := True,
            windowModal := True ]

  widgetSetAppPaintable win True
  setAlpha win
  windowSetPosition win WinPosNone
  windowSetKeepAbove win True
  windowSetRole win "Desktop Fish"
  windowSetHasFrame win False

  widgetShowAll win

-- FIXME: Some kind of flicker on start. Maybe start offscreen?

-- The transparency needs to be redone as the window is redrawn
  winDraw <- widgetGetDrawWindow win
  onExpose win (\_ -> transparentBG winDraw >> return False)

--TODO: Update screen size changed signal

--TODO: be able to start from right / backwards

  scr <- widgetGetScreen win

  gen <- newPureMTSysSeed
  scrSize <- getScreenSize scr

  let (iniPos, gen')   = randomStartPos gen  scrSize  -- random offscreen location to start from
      (iniDest, gen'') = randomPos      gen' scrSize  -- where to go from there (not offscreen)
      iniBw = fst iniDest < fst iniPos

  let iniFishState = FishState { curFrame = 1,
                                 frameN = fishCount,
                                 backwards = iniBw,
                                 speed = defaultSpeed,
                                 dest = iniDest,
                                 screenSize = scrSize,
                                 rndGen = gen'',
                                 frames = fwdFrames,
                                 backFrames = bckFrames
                               }

  move win iniPos

  fishRef <- newIORef iniFishState

  fishIO img win fishRef

  every fishTimeout (fishIO img win fishRef)

  on win buttonPressEvent $
    tryEvent $ liftIO $ readIORef fishRef >>= print

  return win

main :: IO ()
main = do
  initGUI
  fr <- fishFrames fishScale

  createWanda fr
--replicateM_ 100 (createWanda fr)

  mainGUI


