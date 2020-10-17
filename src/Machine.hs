module Machine where

import qualified Data.Vector.Storable as V
import SDL.Vect
import GHC.Word

type Instruction = Word16

data Machine = Machine
  { activePixels :: V.Vector (V2 Int)
  , memory       :: V.Vector Word8
  , registers    :: V.Vector Word8
  , address      :: Word16
  , stack        :: [Word16]
  , delay        :: Int
  , sound        :: Int
  }

screenSize :: V2 Int
screenSize = V2 64 32

newMachine :: Machine
newMachine = Machine
  { activePixels = V.fromList $ map (V2 1) [1..10]
  , memory = V.replicate 0x1000 undefined
  , registers = V.replicate 16 undefined
  , address = undefined
  , stack = undefined
  , delay = undefined
  , sound = undefined
  }
