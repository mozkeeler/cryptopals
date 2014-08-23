import Data.Bits
import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import Data.Word8
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Base64

bitOn n b = if (n .&. b) > 0 then 1 else 0

countBits n = sum (map (\ b -> bitOn n b) bitlist)
  where bitlist = [0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01]

hammingByte :: (Word8,Word8) -> Int
hammingByte (a,b) = countBits (xor a b)

hammingDist :: [Word8] -> [Word8] -> Int
hammingDist a b = sum (map hammingByte (zip a b))

normalizedHamming :: Int -> [Word8] -> Int
normalizedHamming size input = result
  where
    first = take size input
    second = drop size (take (2 * size) input)
    dist = hammingDist first second
    result = quot (100 * dist) size

findSmallestHammings :: [Word8] -> [Int]
findSmallestHammings input = result
  where
    sizes = [3..40]
    hammings = map (\ size -> normalizedHamming size input) sizes
    zipped = zip sizes hammings
    sorted = sortBy (\ (a,b) (c,d) -> compare b d) zipped
    result = map (\ (size,dist) -> size) (take 20 sorted)

charIsOk c = (Data.Char.isPrint c) || (Data.Char.isSpace c)

score s = result
  where
    okChars = filter charIsOk s
    containsBadChars = not ((length okChars) == (length s))
    alphaNumChars = filter Data.Char.isAlphaNum s
    result = if containsBadChars then -1 else (length alphaNumChars)

decrypt :: [Word8] -> Word8 -> String
decrypt l k = result
  where
    decrypted = map (\ e -> xor e k) l
    result = map (\ b -> chr (fromEnum b)) decrypted

decryptSingleXOR bytes = result
  where
    keys = [0..255]
    decryptions = map (\ k -> decrypt bytes k) keys
    scores = map score decryptions
    scoresAndDecryptions = zip scores (zip decryptions keys)
    sorted = sortBy (\ (a,b) (c,d) -> compare c a) scoresAndDecryptions
    result = sorted !! 0

combineTransposed (s,(pt,key)) (scores,(ptarray,keyarray)) = (s + scores,((pt:ptarray),(key:keyarray)))

decryptForKeySize bytes size = result
  where
    split = chunksOf size bytes
    transposed = transpose split
    decrypted = map decryptSingleXOR transposed
    (score,(transposedOut,keys)) = foldr combineTransposed (0,([],[])) decrypted
    result = (score,((transpose transposedOut),keys))

realMain byteString = putStrLn (show result)
  where
    bytes = ByteString.unpack byteString
    --sizes = findSmallestHammings bytes
    sizes = [3..40]
    decryptions = map (\ s -> decryptForKeySize bytes s) sizes
    sorted = sortBy (\ (a,b) (c,d) -> compare c a) decryptions
    result = sorted !! 0

main :: IO()
main = do lines <- ByteString.readFile "6.txt"
          let base64 = Char8.filter (\ c -> not (c == '\n')) lines
          let bytes = decode base64
          (either putStrLn realMain bytes)
