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
  , _pc           :: Word16
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
  where a = fromIntegral $ (m ^. memory) V.! fromIntegral (m ^. pc)
        b = fromIntegral $ (m ^. memory) V.! fromIntegral (m ^. pc + 1)

writeRegister :: Machine -> Int -> Word8 -> Machine
writeRegister m i v = m & registers .~ V.fromList (V.toList (m ^. registers) & element i .~ v)

readRegister :: Machine -> Int -> Word8
readRegister m i = (m ^. registers) V.! i

runInstruction :: Machine -> Instruction -> Machine
runInstruction m 0x00e0 = error "0x00e0"
runInstruction m 0x00ee = error "0x00ee"
runInstruction m i = case digits 16 i of
  [1,_,_,_] -> m & pc .~ (i .&. 0x0fff)

  [2,_,_,_] -> m & stack .~ (m ^. pc):(m ^. stack)

  [3,x,_,_] -> if readRegister m x' == nn then m & pc +~ 2 else m
    where nn = fromIntegral $ i .&. 0x00ff
          x' = fromIntegral x

  [4,x,_,_] -> if readRegister m x' /= nn then m & pc +~ 2 else m
    where nn = fromIntegral $ i .&. 0x00ff
          x' = fromIntegral x

  [5,x,y,0] -> if x == y then m & pc +~ 2 else m

  [6,x,_,_] -> writeRegister m x' nn
    where nn = fromIntegral $ i .&. 0x00ff
          x' = fromIntegral x

  [7,x,_,_] -> writeRegister m x' (readRegister m x' + nn)
    where nn = fromIntegral $ i .&. 0x00ff
          x' = fromIntegral x

  [8,x,y,0] -> writeRegister m x' vy
    where x' = fromIntegral x
          vy = readRegister m $ fromIntegral y

  [8,x,y,1] -> writeRegister m x' (vx .|. vy)
    where x' = fromIntegral x
          vx = readRegister m x'
          vy = readRegister m $ fromIntegral y

  [8,x,y,2] -> writeRegister m x' (vx .&. vy)
    where x' = fromIntegral x
          vx = readRegister m x'
          vy = readRegister m $ fromIntegral y

  [8,x,y,3] -> writeRegister m x' (vx `xor` vy)
    where x' = fromIntegral x
          vx = readRegister m x'
          vy = readRegister m $ fromIntegral y

  [8,x,y,4] -> writeRegister (writeRegister m x' res) 0xf
                $ if hasCarry then 1 else readRegister m 0xf
    where x' = fromIntegral x
          vx = readRegister m x'
          vy = readRegister m $ fromIntegral y
          res = vx + vy
          hasCarry = res < vx

  x         -> error $ "Unimplemented instruction" ++ show x
