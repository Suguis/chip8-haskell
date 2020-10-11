module Rom where

import qualified Data.ByteString as B
import GHC.Word (Word8, Word16)
import Data.List.Split
import Data.Bits

type Rom = [Word16]

loadRom :: FilePath -> IO [Word16]
loadRom p = do
  toW16 . B.unpack <$> B.readFile p
  where toW16 :: [Word8] -> [Word16]
        toW16 = map (\(x:y:[]) -> let a = w x
                                      b = w y in a `shift` 8 .|. b) . chunksOf 2
        w = fromIntegral :: Word8 -> Word16
