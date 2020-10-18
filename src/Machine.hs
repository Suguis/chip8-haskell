{-# LANGUAGE TemplateHaskell #-}
module Machine where

import Rom

import qualified Data.Vector.Storable as V
import Data.Digits
import SDL.Vect
import GHC.Word
import Data.Bits
import Control.Lens
import Numeric (showHex)

type Instruction = Word16

data Machine = Machine
  { _activePixels :: V.Vector (V2 Int)
  , _memory       :: V.Vector Word8
  , _registers    :: V.Vector Word8
  , _pc           :: Int
  , _stack        :: [Word16]
  , _delay        :: Int
  , _sound        :: Int
  }

makeLenses ''Machine

instance Show Machine where
  show m = "Machine {\n"
    ++ "activePixels = " ++ show (m ^. activePixels) ++ "\n"
    ++ "memory = " ++ show (map (`showHex` "") $ V.toList $ m ^. memory) ++ "\n"
    ++ "registers = " ++ show (map (`showHex` "") $ V.toList $ m ^. memory) ++ "\n"
    ++ "pc = " ++ show (m ^. pc) ++ "\n"
    ++ "stack = " ++ show (map (`showHex` "") $ m ^. stack) ++ "\n"
    ++ "delay = " ++ show (m ^. delay) ++ "\n"
    ++ "sound = " ++ show (m ^. sound) ++ "\n"
    ++ "}"

screenSize :: V2 Int
screenSize = V2 64 32

newMachine :: Rom -> Machine
newMachine r = Machine
  { _activePixels = V.fromList $ map (V2 1) [1..10]
  , _memory    = V.replicate 0x1ff 0 V.++ V.fromList r
  , _registers = V.replicate 16 0
  , _pc        = 0x0200
  , _stack     = []
  , _delay     = 0
  , _sound     = 0
  }

nextCycle :: Machine -> Machine
nextCycle m = nm
  where i  = fetchInstruction m
        nm = runInstruction m i & pc +~ 2

fetchInstruction :: Machine -> Word16
fetchInstruction m = a .|. b `shift` 8
  where a = fromIntegral $ (m ^. memory) V.! (m ^. pc)
        b = fromIntegral $ (m ^. memory) V.! (m ^. pc + 1)


runInstruction :: Machine -> Instruction -> Machine
runInstruction m 0x00e0 = error "0x00e0"
runInstruction m 0x00ee = error "0x00ee"
runInstruction m i = case digits 16 i of
  [1,_,_,_] -> error "Jump to address nnn"
  x         -> error $ "Unimplemented instruction" ++ show x
