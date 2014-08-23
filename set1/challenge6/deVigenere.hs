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
    chunks = chunksOf size input
    pairsOfChunks = chunksOf 2 chunks
    dists = map (\ l -> hammingDist (head l) (last l)) pairsOfChunks
    distSum = sum dists
    averageDist = quot (1000 * distSum) (length pairsOfChunks)
    result = quot averageDist size

findSmallestHammings input = result
  where
    sizes = [3..40]
    hammings = map (\ size -> normalizedHamming size input) sizes
    zipped = zip sizes hammings
    sorted = sortBy (\ (a,b) (c,d) -> compare b d) zipped
    result = map fst (take 20 sorted)

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

fstCompare (a,b) (c,d) = compare c a

count :: Word8 -> [Word8] -> Word8
count e (l:ls) = (+) (if (==) e l then 1 else 0) (count e ls)
count e _ = 0

uniq l = foldr (\ e l -> if (elem e l) then l else (e:l)) [] l

mostFrequentBytes l = result
  where
    counts = map (\ e -> count e l) l
    pairs = zip l counts
    result = uniq (sortBy (\ (a,b) (c,d) -> compare d b) pairs)

decryptSingleXOR bytes = result
  where
    frequentBytes = fst (unzip (mostFrequentBytes bytes))
    -- 0x20 is space
    keys = map (\ e -> xor e 0x20) frequentBytes
    decryptions = map (\ k -> decrypt bytes k) keys
    scores = map score decryptions
    scoresAndDecryptions = zip scores (zip decryptions keys)
    sorted = sortBy fstCompare scoresAndDecryptions
    result = sorted !! 0

combineTransposed (s,(pt,key)) (ss,(pts,ks)) = (s + ss,((pt:pts),(key:ks)))

decryptForKeySize bytes size = result
  where
    split = chunksOf size bytes
    transposed = transpose split
    decrypted = map decryptSingleXOR transposed
    (score,(transposedOut,keys)) = foldr combineTransposed (0,([],[])) decrypted
    result = (score,((transpose transposedOut),keys))

realMain byteString = putStrLn result
  where
    bytes = ByteString.unpack byteString
    sizes = findSmallestHammings bytes
    decryptions = map (\ s -> decryptForKeySize bytes s) sizes
    sorted = sortBy fstCompare decryptions
    (score,(lineArray,keyArray)) = sorted !! 0
    result = concat lineArray

main :: IO()
main = do lines <- ByteString.readFile "6.txt"
          let base64 = Char8.filter (\ c -> not (c == '\n')) lines
          let bytes = decode base64
          (either putStrLn realMain bytes)
