{-# LANGUAGE OverloadedStrings #-}
module Main where

import Rom
import Dbg
import Machine
import Render
import SDL

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
  print . stringRom =<< loadRom file

