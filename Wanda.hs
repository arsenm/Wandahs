-- -*-haskell-*-
-- Wanda: A desktop Fish.
--
-- Copyright (C) 2010 Matthew Arsenault

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

{-# LANGUAGE ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -W -funbox-strict-fields #-}
{-# CFILES wanda_image.c #-}

--TODO: Fortune logging?

import Graphics.Rendering.Cairo

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Gdk.EventM

import qualified Data.ByteString.Lazy as B

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Exception

import Data.Array
import Data.Maybe
import Data.Binary.Get
import Data.IORef

import Foreign.Ptr

import System.IO
import System.Process (readProcess)
import System.Environment (getArgs, getProgName)
import System.Random.Mersenne.Pure64
import System.Console.GetOpt

-- | The normal Wanda picture, free of the hassles of installing
-- files.
foreign import ccall "wanda_image.h &wandaimage"
  wandaImage :: Ptr InlineImage

type Pos = (Int, Int)
type Vec = (Int, Int)
type Fish = Window
type Bubble = Window
type FishFrame = (Pixbuf, Bitmap)


--TODO: Better seeding not based off clock. With clock, seem to get
-- wanda schools moving in same general way?

-- | Track the state of the fish
data FishState = FishState { dest :: !Pos,         -- ^ Current destination
                             curFrame :: !Int,     -- ^ Current frame index
                             frameN :: !Int,       -- ^ Total number of frames in the animation
                             backwards :: !Bool,   -- ^ If traveling backwards or not
                             speed :: !Int,        -- ^ Current speed Wanda is traveling
                             origSpeed :: !Int,    -- ^ The original speed
                             fastSpeed :: !Int,    -- ^ The running away fast speed
                             rndGen :: PureMT,     -- ^ Random nubmer generator for this fish
                             speaking :: !Bool,    -- ^ Whether the fish is speaking
                             screenSize :: !(Int,Int),  -- ^ Size of the screen the fish is on
                             frameSize :: !(Int,Int),   -- ^ Height and width of the individual frames
                             frames :: Array Int FishFrame,  -- ^ Frames with fish facing left
                             backFrames :: Array Int FishFrame, -- ^ Frames with fish facing right
                             fishWin :: Fish                    -- ^ The fish's window
                           }


instance Show FishState where
  show s = unlines ["FishState {\n",
                    "\tdest = "        ++ show (dest s),
                    "\tcurFrame = "    ++ show (curFrame s),
                    "\tbackwards = "   ++ show (backwards s),
                    "\tscreenSize = "  ++ show (screenSize s),
                    "\tspeed = "       ++ show (speed s),
                    "\torigSpeed = "   ++ show (origSpeed s),
                    "\tspeaking = "    ++ show (speaking s),
                    "\tframeSize = "   ++ show (frameSize s),
                    "}\n"]

-- | Options for the fish.
data FishOpts = FishOpts { optScale :: Maybe Double,   -- ^ Scale the fish image
                           optIniSpeed :: Int,         -- ^ How fast Wanda should swim in pixels
                           optFastSpeed :: Int,        -- ^ How fast when running away
                           optDisplayMessage :: Bool,  -- ^ Give fortunes when clicked
                           optNumFish :: Int,          -- ^ Number of fish to spawn
                           optTimeout :: Int,          -- ^ Update interval for animation
                           optFortune :: [String],     -- ^ Extra options to pass to fortune
                           optHelp :: Bool,            -- ^ Display help
                           optMode :: FishMode         -- ^ What mode to use
                         } deriving (Eq, Show)

data FishMode = Fortunes
              | Running
              deriving (Eq, Show)

fishCount = 8
fishHeight = 55
fishWidth = 90

defaultSpeed = 10
defaultTimeout = 100
defaultScale = Just 0.5
-- Unfortunately the wrong kind of scale.
-- Fish puns for great justice.
--fishScale = Just 0.5
--fishScale = Nothing

-- | Default fish options
defaultOptions :: FishOpts
defaultOptions = FishOpts { optScale = defaultScale,
                            optIniSpeed = defaultSpeed,
                            optFastSpeed = 2 * defaultSpeed,
                            optDisplayMessage = False,
                            optNumFish = 1,
                            optTimeout = defaultTimeout,
                            optFortune = [],
                            optHelp = False,
                            optMode = Fortunes
                          }

-- | Command line arguments for Wanda.
options :: [OptDescr (FishOpts -> FishOpts)]
options =
  [ Option ['M']     ["display-message"]
    (NoArg (\opts -> opts { optDisplayMessage = True })) "Don't run, give fortunes."

  , Option ['h', '?']     ["help"]
    (NoArg (\opts -> opts { optHelp = True })) "This help."

  , Option ['N']     ["number"]
    (OptArg (\str opts -> opts { optNumFish = maybe 1 read str } ) "Fish count")
    "Number of fish to start"

  , Option ['S']     ["scale"]
    (OptArg (\str opts -> opts { optScale = read <$> str } ) "scale factor")
    "Scale the fish"

  , Option ['v']     ["velocity"]
    (ReqArg (\str opts -> opts { optIniSpeed = read str } ) "normal speed")
    "Normal speed"

  , Option ['t']     ["timeout"]
    (ReqArg (\str opts -> opts { optTimeout = read str } ) "Update interval")
    "Update interval"

  , Option ['f']     ["fast-velocity"]
    (ReqArg (\str opts -> opts { optIniSpeed = read str } ) "Running speed")
    "Running away speed"

  , Option ['F']     ["fortunes"]
    (NoArg (\opts -> opts { optMode = Fortunes } ))
    "Wanda speaks a fortune when clicked. (Default)"

  , Option ['R']     ["runaway"]
    (NoArg (\opts -> opts { optMode = Running } ))
    "Wanda swims away quickly when clicked."

  ]

wandaOpts :: IO FishOpts
wandaOpts = do
  argv <- getArgs
  pn <- getProgName
  let header = "Usage: " ++ pn ++ " [OPTION...] files..." ++ "\n\tAdditional options will be passed to fortune"
  case getOpt' Permute options argv of
          (o, _, n, [])  -> do let o' = (foldl (flip id) (defaultOptions { optFortune = n }) o)
                               when (optHelp o') (error $ usageInfo header options)
                               return o'
          (_, _, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))


-- | No longer speaking, so choose a new direction and destroy the bubble
unsetSpeaking :: Pos -> State FishState ()
unsetSpeaking p = newDest p >> modify (\s -> s { speaking = False })

-- | Handler for clicking on the bubble, which destroys the speech
-- bubble and resumes fish movement.
bubbleClick :: Window           -- ^ The speech bubble's window
            -> IORef FishState  -- ^ The mutable state of the fish
            -> EventM EButton Bool
bubbleClick win ref = liftIO $ do
  p <- windowGetPosition =<< fishWin <$> readIORef ref
  execIOState ref (unsetSpeaking p)
  widgetDestroy win
  return True


-- | Handler for clicking on the fish. Depending on the mode Wanda is
-- in, this could mean displaying a fortune in a speech bubble, or
-- running quickly off the screen.
fishClick :: Fish                 -- ^ The fish window
          -> IORef FishState      -- ^ The mutable fish state
          -> [String]             -- ^ Additional arguments to pass to fortune
          -> FishMode             -- ^ What the fish should do on click
          -> EventM EButton Bool
fishClick fish fsRef args mode = liftIO $ do
  p <- windowGetPosition fish
  case mode of
    Fortunes -> createSpeechBubble fsRef p args >> return ()
    Running  -> runAway fsRef p
  return True

runAway :: IORef FishState -> Pos -> IO ()
runAway ref p = execIOState ref (setRunning p)

setRunning :: Pos -> State FishState ()
setRunning p = setFastSpeed >> newOffscreenDest p



-- | Read an initial state from an IORef, run through the state
-- monad. Update the state of the state monad to the resulting state,
-- and return the result.
-- Basically it's modifyIORef with the result of the state monad.
runIOState :: IORef a -> State a b -> IO b
runIOState ref f = do
  (v, st') <- runState f <$> readIORef ref
  writeIORef ref st'
  return v

-- | Use an initial state read from an ioref, and update after.
execIOState ref m = modifyIORef ref (execState m)


--FIXME: The bubble placement could be smarter. There are also cases
--with very large fortunes (which are pretty rare) that without
--moving, the bubble can't fit on screen. Maybe scale the text?

-- | Adds a bubble to the given fish state.  Returns the place where
-- the point needs to go.
addBubble :: Pos             -- ^ Current fish position
          -> (Int, Int)      -- ^ Size of the bubble
          -> State FishState (BubbleDir, Pos) -- ^ Where the bubble should be placed on screen, and point direction
addBubble (x,y) (bw, bh) = do
  (sw, _)  <- gets screenSize
  bckw     <- gets backwards
  (fw, fh) <- gets frameSize

  -- TODO: Account for bubble placement towards mouth
  let l = x >= bw             -- Will fit towards the left
      r = x + bw + fw <= sw   -- Will fit towards the right

      u = y >= bh             -- Will fit above
      --d = y + bh + fh <= sh   -- Will fit below

     -- Prefer left/right in same direction as travel. Second item is
     -- if a flip is needed
      horizF | bckw      = if l then (R, False) else (L, True)
             | otherwise = if r then (L, False) else (R, True)

      vert = if u then D else U  -- prefer upper bubbles
      horiz = fst horizF

     -- Positioning of the bubble towards the mouth etc.
      bp = (bx,by)
      mfx = (2 * fw) `div` 11  -- Mouth seems to be 2/11ths across the frame
      mfy = (5 * fh) `div` 7   -- Mouth seems about 5/7s down the frame
      bx = if horiz == L  -- The bubble points left, so put bubble on the right of Wanda.
              then x + fw - mfx
              else x - bw + mfx
      by = if vert == D   -- Point is down, so shift up
             then y - bh + mfy
             else y + mfy

  -- Update the fish state if we need to turn around.
  when (snd horizF) swapDirection
  setSpeaking
  return (Dir horiz vert, bp)

-- | Create a new speech bubble window, displaying a fortune from
-- 'fortune'. The window is placed appropriately near the fish, which
-- is set to speaking. The bubble has the text centered, and has the
-- pointy part placed appropriately to try making sure the fortune is
-- on screen.
createSpeechBubble :: IORef FishState  -- ^ The mutable fish state
                   -> Pos              -- ^ The current position of the fish
                   -> [String]         -- ^ Additional arguments to pass to fortune
                   -> IO Bubble        -- ^ The new speech bubble window
createSpeechBubble ref pos args = do
  -- Wanda speaks. Figure out the size of the window from the size.
  txt   <- readProcess "fortune" args ""
  pctx  <- cairoCreateContext Nothing
  lay   <- layoutText pctx txt
  fsize <- setFont lay

  (PangoRectangle x y w' h',_) <- layoutGetExtents lay

  --extra space for the pointy bit, plus padding
  let wt = w' - x       -- width of the text
      ht = h' - y       -- height of the text
      yoff = 2 * fsize  -- padding area
      xoff = 3 * fsize

      boxw = wt + (2 * xoff)  -- size of the padded rectangle area
      boxh = ht + (2 * yoff)

      px  = boxw / 10    -- pointy bit
      py  = boxh / 5
      rpx = xoff         -- how far back to the side for it
      rpy = yoff

      ww = floor $ boxw + px  -- Padded area + pointy bit, the whole surface
      wh = floor $ boxh + py

  putStrLn txt

  win <- windowNew
  set win [ windowTitle := "Wanda the Fish Says",
            windowDecorated := False,
            windowTypeHint := WindowTypeHintDock, -- Dock
            windowGravity := GravityStatic,     -- Importantish.
            windowDefaultWidth := ww,
            windowDefaultHeight := wh,
            windowAcceptFocus := False,
            windowResizable := True,
            windowSkipTaskbarHint := True,
            windowSkipPagerHint := True,
            windowModal := True ]

  widgetSetAppPaintable win True
  setAlpha win
  windowSetPosition win WinPosNone
  windowSetKeepAbove win True
  windowSetRole win "Wanda Says"
  windowSetHasFrame win False

  -- Add the bubble to the fish. Also figures out the direction to
  -- draw the speech bubble.
  (dir, bp) <- runIOState ref (addBubble pos (ww,wh))

  move win bp

  -- This bitmap is used for applying the bubble input shape to the window
  bm <- pixmapNew (Nothing::Maybe DrawWindow) ww wh (Just 1)

  win `on` exposeEvent $ drawSpeech lay win bm boxw boxh xoff yoff px py rpx rpy dir
  win `on` screenChanged $ \_ -> setAlpha win
  win `on` buttonPressEvent $ bubbleClick win ref

  widgetShowAll win
  return win

-- | Sets the font of the layout, and returns the font size
setFont :: PangoLayout  -- ^ The layout to set the font of
        -> IO Double    -- ^ The font size
setFont lay = do
  fd <- fontDescriptionFromString "sans monospace 10"
  fontDescriptionSetWeight fd WeightBold
  layoutSetFontDescription lay (Just fd)
  fromJust <$> fontDescriptionGetSize fd

-- | Position of the pointy part
data BubbleDir = Dir Horiz Vert
               deriving (Eq)

-- | Point on Left or Right
data Horiz = L | R deriving (Eq)

-- | Point on upper or lower corner
data Vert = U | D deriving (Eq)


-- | Drawing function for the speech bubble.
drawSpeech :: PangoLayout   -- ^ Layout containing the fortune to display
           -> Bubble        -- ^ The Bubble's GTK window
           -> Bitmap        -- ^ Bitmap for shaped input
           -> Double        -- ^ Width of the box area, neglecting the point part
           -> Double        -- ^ Height of the box area, neglecting the pointy part
           -> Double        -- ^ Offset / padding / margin in the x direction
           -> Double        -- ^ Offset / padding / margin in the y direction
           -> Double        -- ^ Distance in the x direction for the point
           -> Double        -- ^ Distance in the y direction for the point
           -> Double        -- ^ What point the pointy part should come back to on the bubble
           -> Double        -- ^ What point the pointy part should come back to on the bubble
           -> BubbleDir     -- ^ Which way the point goes
           -> EventM EExpose Bool
drawSpeech lay bub bm w h xoff yoff px py rpx rpy dir = do
  let r = 40  -- Arbitrary number
      x = 0
      y = 0

      -- The offset of the text must change depending on where the bubble is
      (bdr, tx, ty) = case dir of
                        Dir R U -> (ruBubble, x+px, y+py+yoff)
                        Dir L U -> (luBubble, x+px+xoff, y+py+yoff)
                        Dir R D -> (rdBubble, x+xoff, y+yoff)
                        Dir L D -> (ldBubble, x+px+xoff, y+yoff)

      ruBubble = do  -- Bubble with point on upper right corner
        let y' = y + py  -- Everything is shifted by the point's space
            x2 = x + w
            y2 = y' + h
        -- start from upper right, below the point start
        moveTo x2 (y'+ rpy)
        lineTo x2 (y2-r)

        -- Curve on lower right corner
        curveTo x2 y2 x2 y2 (x2-r) y2
        lineTo (x+px) y2

        -- Curve on lower left corner
        curveTo x y2 x y2 x (y2 -r)
        lineTo x (y' + r)

        -- Curve on upper left
        curveTo x y' x y' (x+r) y'
        lineTo (x2-rpx) y'

        -- pointy bit in upper right
        lineTo (x2+px) y

      luBubble = do  -- Bubble with point on upper left corner
        let x' = x + px
            y' = y + py
            x2 = x' + w  -- upper limits of the box
            y2 = y' + h
        -- Start from upper left
        moveTo (x' + rpx) y'
        lineTo (x' + w - r) y'

        -- Curve down on the upper right
        curveTo x2 y' x2 y' x2 (y' + r)
        lineTo x2 (y2 - r)

        -- Curve back to the left on the lower right corner
        curveTo x2 y2 x2 y2 (x2 - r) y2
        lineTo (x' + rpx) y2

        -- curve on lower left corner
        curveTo x' y2 x' y2 x' (y2 - r)
        lineTo x' (y' + rpy)

        -- pointy bit
        lineTo x y

      ldBubble = do        -- Draw a speech bubble with point on the lower left
        let x' = x + px
            x2 = x' + w
            y2 = y + h
        -- Start from lower left
        moveTo x' (y2 - py)
        lineTo x' (y+r)

        -- Wrap around upper left corner
        curveTo x' y x' y (x' + r) y
        lineTo (x' + w - r) y

        -- Curve down on the upper right
        curveTo x2 y x2 y x2 (y+r)
        lineTo x2 (y2-r)

        -- Curve back to the left on the lower right corner
        curveTo x2 y2 x2 y2 (x2 - r) y2
        lineTo (x' + rpx) y2

        -- pointy bit
        lineTo x (y2+py)

      rdBubble = do        -- Draw a speech bubble with point on the lower right
        let y2 = y + h
            x2 = x + w
        -- start from bottom left
        moveTo (x2-rpx) (y+h)
        lineTo (x+r) (y+h)
        curveTo x y2 x y2 x (y2-r)
        lineTo x (y+r)

        -- upper left
        curveTo x y x y (x+r) y
        lineTo (x2-r) y

        -- upper right
        curveTo x2 y x2 y x2 (y+r)

        -- pointy bit
        lineTo x2 (y2 - rpy)
        lineTo (x2+px) (y2+py)

      drawBubble bubblePath = do
        -- Fill the bubble background
        setOperator OperatorSource
        setSourceRGBA 1 1 1 0
        paint

        -- Draw the border
        setSourceRGB 0 0 0
        setLineWidth 0
        setLineCap LineCapRound
        setLineJoin LineJoinRound

        bubblePath
        closePath

        setSourceRGBA 0.9 0.9 0.9 0.8
        fill

        -- Set color, and draw the text
        setSourceRGB 0 0 0
        moveTo tx ty
        showLayout lay

  win <- eventWindow
  let rndr = drawBubble bdr
  liftIO $ do renderWithDrawable win rndr
              renderWithDrawable bm rndr
              widgetInputShapeCombineMask bub (Just bm) 0 0
  return True

-- | Set a widget to use an RGBA colormap
setAlpha :: (WidgetClass widget) => widget -> IO ()
setAlpha widget = do
  screen <- widgetGetScreen widget
  colormap <- screenGetRGBAColormap screen
  maybe (return ()) (widgetSetColormap widget) colormap

setScreenSize :: (Int, Int) -> State FishState ()
setScreenSize ss = modify (\s -> s { screenSize = ss })

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
splitStrip :: Maybe Double     -- ^ A scale to use. If @Nothing@ the image is left its original size
           -> Int              -- ^ Number of frames contained in the strip
           -> Pixbuf           -- ^ The source image
           -> IO (Array Int FishFrame, Array Int FishFrame)  -- ^ Resulting arrays
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
-- animation.
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
-- | Main fish state change function. Updates the state and returns
-- the new position to set.
fishTick :: Pos                 -- ^ The current position of the fish
         -> State FishState Pos -- ^ The new position of the fish
fishTick p@(x,y) = do
  spd     <- gets speed
  d@(t,_) <- gets dest
  spk     <- gets speaking

  let (dx,dy) = vec spd p d
      x'      = x + dx
      p'      = (x', y + dy)
      close   = dist p' d <= spd
      stuck   = dx == 0 && dy == 0
      --CHECKME: Can stuck actually happen?

      fishMove = do
        -- Not on same side after moving, so turned around
        when ( (t < x) /= (t < x') ) swapDirection

        -- Avoid getting stuck, and get close enough.
        -- If Wanda is running away, slow back down.
        when (stuck || close) (do newDest p'
                                  fast <- gets fastSpeed
                                  when (spd == fast) restoreSpeed)
        return p'

  updateFrame

  if spk    -- Wanda swims in place when speaking
     then return p
     else fishMove


-- | Move the current frame to the next
updateFrame :: State FishState ()
updateFrame = do
  i <- gets curFrame
  n <- gets frameN
  let i' = (i + 1) `mod` n
  modify (\s -> s { curFrame = i' })

-- | Choose a location far away offscreen place towards the closest
-- screen edge, for running away.
newOffscreenDest :: Pos -> State FishState ()
newOffscreenDest (x,y) = do
  (sx, sy) <- gets screenSize
  gen      <- gets rndGen
  oldBw    <- gets backwards

  let (tx1,ty1)   = (2 * sx, 2 * sy)
      (tx2,ty2)   = (4 * sx, 4 * sy)
      xBnds       = if dx2 < dx1    -- right is closer
                       then (sx + tx1, sx + tx2)
                       else (negate tx2, negate tx1)
      yBnds       = if dy2 < dy1
                       then (sy + ty1, sy + ty2)
                       else (negate ty2, negate ty1)
      (loc, gen') = randomPair gen xBnds yBnds

      dx1 = x       -- distance from left of screen
      dx2 = sx - x  -- distance from right

      dy1 = y       -- distance to top
      dy2 = sy - y  -- distance to bottom

      bw = fst loc < x

  when (oldBw /= bw) swapDirection
  modify (\s -> s { rndGen = gen',
                    dest = loc
                  } )

-- | Pick a new 'normal' destination and Randomgen from the current
-- position
newDest :: Pos -> State FishState ()
newDest (x,_) = do
  (sx, sy) <- gets screenSize
  gen      <- gets rndGen
  oldBw    <- gets backwards

  -- limits are (screen x +- 10%, screen y +- 10%)
  let (tx,ty)     = (sx `div` 10, sy `div` 10)
      xBnds       = (negate tx, sx + tx)
      yBnds       = (negate ty, sy + ty)
      (loc, gen') = randomPair gen xBnds yBnds
      bw = fst loc < x

  when (bw /= oldBw) swapDirection
  modify (\s -> s { rndGen = gen',
                    dest = loc
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
vec :: Int   -- ^ The approximate length of the vector to produce
    -> Pos   -- ^ Starting point
    -> Pos   -- ^ Ending point
    -> Vec   -- ^ Resulting (dx, dy) vector from point 1 to point 2
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

-- | Produce a new mersenne-random-pure64 random number generator,
-- seeding from /dev/urandom. If that fails, i.e. on Windows, just use
-- the default seeded from clock newPureMT.
newPureMTSysSeed :: IO PureMT
newPureMTSysSeed =
  handle (\(_::IOException) -> newPureMT) $
    bracket (openBinaryFile "/dev/urandom" ReadMode)
            hClose
            (\h -> pureMT . runGet getWord64le <$> B.hGet h 8)


-- | Generate a random integer in the given range using
-- mersenne-random-pure64. I don't understand why that doesn't have
-- such a function already.
randomIntR :: (Int, Int)  -- ^ The range to use
           -> PureMT -> (Int, PureMT)
randomIntR (l,u) s = let n = u - l + 1
                         (v, s') = randomInt s
                     in (l + v `mod` n, s')

-- | Read the frame from the appropriate array depending on travel
-- direction
getFrame :: FishState -> FishFrame
getFrame s = let arr = if backwards s
                         then backFrames s
                         else frames s
             in arr ! curFrame s

move :: Fish -> Pos -> IO ()
move = uncurry . windowMove

setSpeaking :: State FishState ()
setSpeaking = modify (\s -> s { speaking = True })

setFastSpeed :: State FishState ()
setFastSpeed = modify (\s -> s { speed = fastSpeed s })

restoreSpeed :: State FishState ()
restoreSpeed = modify (\s -> s { speed = origSpeed s })


-- | Perform the IO needed for a fish update, i.e. move the window and
-- update the image
fishIO :: Image            -- ^ The fish image widget
       -> Fish             -- ^ The fish window
       -> IORef FishState  -- ^ The mutable fish state
       -> IO Bool          -- ^ Continue the signal
fishIO img win ref = do
  r  <- windowGetPosition win
  (r', fs') <- runState (fishTick r) <$> readIORef ref
  move win r'
-- moveBub r' fs'

  let (fr,msk) = getFrame fs'
  imageSetFromPixbuf img fr

--TODO: Do I need to remove the old mask?
--It appears I do, at least for the input shape. I get gdk CRITICAL if I don't
-- set update clickthrough
-- TODO: Maybe do this if no compositing available
--  widgetShapeCombineMask win Nothing 0 0
--  widgetShapeCombineMask win (Just msk) 0 0

  widgetInputShapeCombineMask win Nothing    0 0
  widgetInputShapeCombineMask win (Just msk) 0 0

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

-- | Convenience for timeoutAdd
every = flip timeoutAdd

-- | Get the screen size as a pair
getScreenSize :: Screen -> IO (Int, Int)
getScreenSize scr = liftA2 (,) (screenGetWidth scr) (screenGetHeight scr)

-- | Get the height and width of a pixbuf for convenience
pixbufGetSize :: Pixbuf -> IO (Int, Int)
pixbufGetSize p = liftA2 (,) (pixbufGetWidth p) (pixbufGetHeight p)

-- | Split the image up into the individual fish frames
fishFrames :: Maybe Double -> IO (Array Int FishFrame, Array Int FishFrame)
fishFrames scale = splitStrip scale fishCount =<< pixbufNewFromInline wandaImage

-- | Get the Bitmap for a pixbuf to allow clickthrough of transparent
-- parts.
getMask :: Pixbuf -> IO Bitmap
getMask pb = do
  w <- pixbufGetWidth  pb
  h <- pixbufGetHeight pb
  bm <- pixmapNew (Nothing::Maybe DrawWindow) w h (Just 1)
  pixbufRenderThresholdAlpha pb bm 0 0 0 0 (-1) (-1) 1
  return bm


-- | Takes the (forward frames, backward frames) and creates a new wanda window
createWanda :: FishOpts  -- ^ Options for the fish
            -> (Array Int FishFrame, Array Int FishFrame)  -- ^ The forward and backwards frame arrays
            -> IO Fish    -- ^ A new fish window
createWanda o (bckFrames, fwdFrames) = do
  img <- imageNew
  widgetSetDoubleBuffered img True

 -- Setup the window. Needs to be drawable for transparency to work.
  win <- windowNew
  set win [ containerChild := img,
            windowTitle := "Wanda",
            windowDecorated := False,
            windowTypeHint := WindowTypeHintDock,
            windowGravity := GravityStatic,     -- Importantish.
            windowDefaultHeight := fishHeight,
            windowDefaultWidth := fishWidth,
            windowAcceptFocus := False,
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

-- The transparency needs to be redone as the window is redrawn
  winDraw <- widgetGetDrawWindow win
  onExpose win (\_ -> transparentBG winDraw >> return False)

--TODO: Update screen size changed signal

--TODO: be able to start from right / backwards

  gen <- newPureMTSysSeed
  scrSize <- getScreenSize =<< widgetGetScreen win
  imgSize <- pixbufGetSize (fst $ fwdFrames ! 0) -- kind of shittily get the size after scaling

  let (iniPos, gen')   = randomStartPos gen  scrSize  -- random offscreen location to start from
      (iniDest, gen'') = randomPos      gen' scrSize  -- where to go from there (not offscreen)
      iniBw = fst iniDest < fst iniPos

      iniFishState = FishState { curFrame = 1,
                                 frameN = fishCount,
                                 backwards = iniBw,
                                 speed = optIniSpeed o,
                                 origSpeed = optIniSpeed o,
                                 fastSpeed = optFastSpeed o,
                                 dest = iniDest,
                                 screenSize = scrSize,
                                 frameSize = imgSize,
                                 speaking = False,
                                 rndGen = gen'',
                                 frames = fwdFrames,
                                 backFrames = bckFrames,
                                 fishWin = win
                               }
  move win iniPos

  fishRef <- newIORef iniFishState

  fishIO img win fishRef

  every (optTimeout o) (fishIO img win fishRef)

  win `on` screenChanged $ \scr -> do setAlpha win
                                      execIOState fishRef . setScreenSize =<< getScreenSize scr
  win `on` buttonPressEvent $ fishClick win fishRef (optFortune o) (optMode o)

  return win

-- | Main is main. Make Wanda go.
main :: IO ()
main = do
  initGUI
  o  <- wandaOpts
  fr <- fishFrames (optScale o)

  replicateM_ (optNumFish o) (createWanda o fr)

  mainGUI

