{-# LANGUAGE OverloadedStrings #-}
module Main where

import Rom
import Dbg
import Machine

import SDL
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "CHIP-8" WindowConfig
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
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer newMachine
  let file = "roms/pong.ch8"
  print =<< stringRom <$> loadRom file

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
