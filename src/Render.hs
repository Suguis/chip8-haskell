{-# LANGUAGE OverloadedStrings #-}
module Render where

import Machine

import qualified Data.Vector.Storable as V
import Foreign.C.Types
import SDL
import Control.Lens

activePixelsSDL :: Machine -> V.Vector (Point V2 CInt)
activePixelsSDL = V.map (\(V2 x y) -> P $ V2 (fromIntegral x) (fromIntegral y)) . (^. activePixels)

renderWindow :: IO Window
renderWindow = createWindow "CHIP-8" WindowConfig
    { windowBorder = True
    , windowHighDPI = False
    , windowInputGrabbed = False
    , windowMode = Windowed
    , windowGraphicsContext = NoGraphicsContext
    , windowPosition = Wherever
    , windowResizable = False
    , windowInitialSize = V2 64 32
    , windowVisible = True
    }

mainRenderer :: IO Renderer
mainRenderer = do
  initializeAll
  window <- renderWindow
  render <- createRenderer window (-1) defaultRenderer
  return render
