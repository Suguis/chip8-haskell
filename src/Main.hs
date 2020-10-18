module Main where

import Rom
import Dbg
import Machine
import Render

main :: IO ()
main = do
  let file = "roms/pong.ch8"
  print . stringRom =<< loadRom file
  m <- newMachine <$> loadRom file
  r <- mainRenderer
  mainLoop r m
