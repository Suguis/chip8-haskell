module Machine where

import qualified Data.Vector.Storable as V
import SDL.Vect
import GHC.Word

data Machine = Machine
  { activePixels :: V.Vector (V2 Int)
  , memory       :: V.Vector Word8
  }

screenSize :: V2 Int
screenSize = V2 64 32

newMachine :: Machine
newMachine = Machine
  { activePixels = V.fromList $ map (V2 1) [1..10]
  , memory = V.replicate 0x1000 0
  }
