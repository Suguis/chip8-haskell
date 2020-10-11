module Main where

import Rom

import System.Environment
import Numeric (showHex)
import GHC.Word (Word8, Word16)
import Data.Bits

main :: IO ()
main = do
  let file = "roms/pong.ch8"
  print =<< stringRom <$> loadRom file

stringRom :: Rom -> [String]
stringRom = map (`showHex` "")
