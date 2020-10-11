module Dbg where

import Rom

import Numeric (showHex)

-- Used to convert a Rom into a printable list of Strings
stringRom :: Rom -> [String]
stringRom = map (`showHex` "")
