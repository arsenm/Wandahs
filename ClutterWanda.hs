module Main where

--import Data.Array
import Control.Applicative

import Graphics.UI.Gtk
import Graphics.UI.Clutter
import qualified Graphics.UI.Clutter as C

import Graphics.X11
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Window

--TODO: Make sizes scale and such
fishWidth, fishHeight, numFishFrames :: Int
fishWidth = 90
fishHeight = 55
numFishFrames = 8

--TODO: pixbufNewFromInline

{-
-- | Split up the image file into animation frames, and flip to go left to right.
getFishFrame :: Pixbuf -> Int -> IO Pixbuf
getFishFrame wholeAnim frame = do
  newFrame <- pixbufNew ColorspaceRgb True 8 fishWidth fishHeight
  pixbufCopyArea wholeAnim (frame * fishWidth) 0 fishWidth fishHeight newFrame 0 0
  pixbufFlipHorizontally newFrame

--Tim unsea'd the fish in the image
-- | Split each frame into an array of pixbufs
getAllFishFrames :: Pixbuf -> IO (Array Int Pixbuf)
getAllFishFrames pb = listArray (1, numFishFrames) <$> mapM (getFishFrame pb) [1..numFishFrames]
-}
--screenGetWidth
--screenGetHeight
--screenWidth
--screenHeight

main = do
  clutterInit

  stage <- stageGetDefault

--  wandaAnim <- pixbufNewFromFile "wanda.png"
  wandaAnim <- textureNewFromFile "wanda.png"

  set stage [ actorSize := (fromIntegral $ numFishFrames * fishWidth, fromIntegral fishHeight),
              actorOpacity := 0,
              stageColor := C.Color 255 255 255 0 ]
  stageHideCursor stage



  containerAddActor stage wandaAnim


  actorShowAll stage



  clutterMain

