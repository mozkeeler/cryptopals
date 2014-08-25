import Crypto.Cipher
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Base64
import Data.List.Split
import Data.Word8

stringToKey string = result
  where
    keyBytes = Char8.pack string
    Right key = makeKey keyBytes :: Either KeyError (Key AES128)
    result = key

cbcBlock cipher prev block = result
  where
    blockBytes = ByteString.unpack block
    prevBytes = ByteString.unpack prev
    input = zip prevBytes blockBytes
    xored = map (\ (a,b) -> xor a b) input
    result = ecbEncrypt cipher (ByteString.pack xored)

cbc cipher iv blocks = result
  where
    result = foldr (\ b -> cbcBlock cipher b) iv blocks

pkcs7pad :: Int -> [Word8] -> [Word8]
pkcs7pad len bytes = map toEnum result
  where
    chunks = chunksOf len (map fromEnum bytes)
    lst = last chunks
    needed = len - length lst
    padding = repeat needed
    paddedLast = concat [lst,(take needed padding)]
    chunksMinusLast = take (length chunks - 1) chunks
    result = concat (chunksMinusLast ++ [paddedLast])

realMain input = Char8.unpack result
  where
    bytes = ByteString.unpack input
    bytesPadded = pkcs7pad 16 bytes
    blocks = map ByteString.pack (chunksOf 16 bytesPadded)
    key = stringToKey "YELLOW SUBMARINE"
    aes = cipherInit key
    iv = ByteString.pack (take 16 (repeat 0))
    encryptedBlocks = cbc aes iv blocks
    result = encode encryptedBlocks

main :: IO()
main = do lines <- readFile "input.txt"
          putStrLn (realMain (Char8.pack lines))
