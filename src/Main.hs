module Main where

import Rom
import Dbg
import Machine
import Render

import Control.Monad (unless)
import Numeric (showHex)
import SDL

main :: IO ()
main = do
  let file = "roms/pong.ch8"
  print . stringRom =<< loadRom file
  m <- newMachine <$> loadRom file
  r <- mainRenderer
  mainLoop r m

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
  print $ showHex (fetchInstruction m) ""
  let nm = nextCycle m

  unless qPressed (mainLoop r nm)
