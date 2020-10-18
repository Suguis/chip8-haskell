{-# LANGUAGE OverloadedStrings #-}
module Render where

import Machine

import qualified Data.Vector.Storable as V
import Foreign.C.Types
import SDL
import Control.Monad (unless)
import Control.Lens

mainLoop :: Renderer -> Machine -> IO ()
mainLoop r m = do
  es <- pollEvents
  let eventIsQPress e =
        case eventPayload e of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress es
  rendererDrawColor r $= V4 255 255 255 255
  drawPoints r $ activePixelsSDL m
  present r
  let nm = nextCycle m
  unless qPressed (mainLoop r nm)

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
