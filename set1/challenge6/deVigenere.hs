import Data.Bits
import Data.Either
import Data.List
import Data.Word8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base64

bitOn n b = if (n .&. b) > 0 then 1 else 0
countBits n = sum (map (\ b -> bitOn n b) bitlist)
  where bitlist = [0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01]
hammingByte (a,b) = countBits (xor a b)
hammingDist a b = sum (map hammingByte (zip a b))
normalizedHamming size input = result
  where
    first = take size input
    second = drop size (take (2 * size) input)
    dist = hammingDist first second
    result = quot (100 * dist) size
findSmallestHammings input = result
  where
    sizes = [3..40]
    hammings = map (\ size -> normalizedHamming size input) sizes
    zipped = zip sizes hammings
    sorted = sortBy (\ (a,b) (c,d) -> compare b d) zipped
    result = map (\ (size,dist) -> size) (take 3 sorted)

filterNewlines c = not (c == '\n')

realMain bytes = putStrLn (show result)
  where
    result = findSmallestHammings (B.unpack bytes)

main :: IO()
main = do lines <- B.readFile "6.txt"
          let base64 = C.filter (\ c -> not (c == '\n')) lines
          let bytes = decode base64
          (either putStrLn realMain bytes)
