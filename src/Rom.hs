module Rom where

import qualified Data.ByteString as B
import GHC.Word (Word8)

type Rom = [Word8]

loadRom :: FilePath -> IO [Word8]
loadRom p = B.unpack <$> B.readFile p
