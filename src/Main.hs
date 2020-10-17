module Main where

import Rom
import Dbg
import Machine
import Render

main :: IO ()
main = do
  -- Debugging rom instructions
  let file = "roms/pong.ch8"
  print . stringRom =<< loadRom file

  let m = newMachine
  r <- mainRenderer
  mainLoop r m
