import Data.Bits
import Data.Char
import Data.List.Split

allhex s = all isHexDigit s
combineHex [x,y] = shiftL x 4 + y

hex2intList h = result
  where
    hexPairs = chunksOf 2 (map digitToInt h)
    result = map combineHex hexPairs

nibble2Hex n = (concat [['0'..'9'],['a'..'f']]) !! n
byte2hex b = [nibble2Hex (shiftR b 4)] ++ [nibble2Hex (b .&. 15)]

xorPair (x,y) = xor x y

doit l1 l2 = result
  where
    xored = map xorPair (zip (hex2intList l1) (hex2intList l2))
    hexbytes = map byte2hex xored
    result = concat hexbytes

validate l1 l2 func = result
  where
    sameLength = (==) (length l1) (length l2)
    allHex = (&&) (allhex l1) (allhex l2)
    evenLength = (==) (mod (length l1) 2) 0
    result = if (&&) ((&&) sameLength allHex) evenLength
      then func l1 l2
      else "Lists not the same, even length or not all hex"

main :: IO ()
main = do l1 <- getLine
          l2 <- getLine
          putStr ((validate l1 l2 doit) ++ "\n")
