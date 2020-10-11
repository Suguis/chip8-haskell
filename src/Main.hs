import qualified Data.ByteString as B
import System.Environment
import Numeric (showHex)
import GHC.Word (Word8, Word16)
import Data.Bits

main :: IO ()
main = do
  --file <- head <$> getArgs
  let file = "roms/pong.ch8"
  contents <- w8Tow16List . B.unpack <$> B.readFile file
  print $ map (`showHex` "") contents
  where w8Tow16List :: [Word8] -> [Word16]
        w8Tow16List []       = []
        w8Tow16List (x:[])   = [shift (w8Tow16 x) 8]
        w8Tow16List (x:y:xs) = let a = w8Tow16 x
                                   b = w8Tow16 y in (shift a 8 .|. b) : w8Tow16List xs
        w8Tow16 = fromIntegral :: Word8 -> Word16
