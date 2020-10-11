module Main where

import Rom
import Dbg

main :: IO ()
main = do
  let file = "roms/pong.ch8"
  print =<< stringRom <$> loadRom file
