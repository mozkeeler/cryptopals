import Crypto.Cipher
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Base64
import Data.List.Split

stringToKey string = result
  where
    keyBytes = Char8.pack string
    Right key = makeKey keyBytes :: Either KeyError (Key AES128)
    result = key

realMain byteString = putStrLn result
  where
    bytes = ByteString.unpack byteString
    blocks = map ByteString.pack (chunksOf 16 bytes)
    key = stringToKey "YELLOW SUBMARINE"
    aes = cipherInit key
    plaintextBlocks = map (\ block -> ecbDecrypt aes block) blocks
    stringBlocks = map Char8.unpack plaintextBlocks
    result = concat stringBlocks

main :: IO()
main = do lines <- ByteString.readFile "7.txt"
          let base64 = Char8.filter (\ c -> not (c == '\n')) lines
          let bytes = decode base64
          (either putStrLn realMain bytes)
