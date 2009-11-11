module Main where

import Data.Array
import Control.Applicative

import Graphics.UI.Gtk
import Graphics.X11
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Window


fishWidth, fishHeight, numFishFrames :: Int
fishWidth = 20
fishHeight = 10
numFishFrames = 8

--TODO: pixbufNewFromInline

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

--screenGetWidth
--screenGetHeight
--screenWidth
--screenHeight

main = do
  initGUI

  wandaAnim <- pixbufNewFromFile "wanda.png"

  mainGUI

