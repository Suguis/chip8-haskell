module Machine where

import Rom

import qualified Data.Vector.Storable as V
import Data.Digits
import SDL.Vect
import GHC.Word

type Instruction = Word16

data Machine = Machine
  { activePixels :: V.Vector (V2 Int)
  , memory       :: V.Vector Word8
  , registers    :: V.Vector Word8
  , pc           :: Word16
  , stack        :: [Word16]
  , delay        :: Int
  , sound        :: Int
  }

screenSize :: V2 Int
screenSize = V2 64 32

newMachine :: Rom -> Machine
newMachine r = Machine
  { activePixels = V.fromList $ map (V2 1) [1..10]
  , memory    = V.fromList r
  , registers = V.replicate 16 undefined
  , pc        = 0x0200
  , stack     = undefined
  , delay     = undefined
  , sound     = undefined
  }

runInstruction :: Machine -> Instruction -> Machine
runInstruction m 0x00e0 = error "0x00e0"
runInstruction m 0x00ee = error "0x00ee"
runInstruction m i = case digits 16 i of
  [1,_,_,_] -> error "Jump to address nnn"
  _         -> error "Unimplemented instruction"
