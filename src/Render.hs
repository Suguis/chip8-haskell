module Render where

import Machine

import qualified Data.Vector.Storable as V
import Foreign.C.Types
import SDL
import Control.Monad (unless)

appLoop :: Renderer -> Machine -> IO ()
appLoop renderer machine = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 255 255 255 255
  drawPoints renderer $ activePixelsSDL newMachine
  present renderer
  unless qPressed (appLoop renderer machine)

activePixelsSDL :: Machine -> V.Vector (Point V2 CInt)
activePixelsSDL = V.map (\(V2 x y) -> P $ V2 (fromIntegral x) (fromIntegral y)) . activePixels
