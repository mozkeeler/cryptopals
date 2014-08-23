import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Base64
import Data.Either
import Data.List
import Data.List.Split

count e (l:ls) = (+) (if e == l then 1 else 0) (count e ls)
count e _ = 0

countDuplicates byteString = result
  where
    bytes = ByteString.unpack byteString
    blocks = map ByteString.pack (chunksOf 16 bytes)
    counts = map (\ e -> count e blocks) blocks
    result = sum counts

main :: IO()
main = do lines <- ByteString.readFile "8.txt"
          let base64s = ByteString.split 0x0a lines
          let bytess = rights (map decode base64s)
          let counts = map countDuplicates bytess
          let countIndexPairs = zip counts [0..]
          let sorted = sortBy (\ (a,b) (c,d) -> compare c a) countIndexPairs
          let index = snd (head sorted)
          let aesBytes = bytess !! index
          let result = encode aesBytes
          putStrLn (show result)
