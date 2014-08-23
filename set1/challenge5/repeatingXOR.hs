import Data.Bits
import Data.Char
import Data.List.Split

nibble2Hex n = (concat [['0'..'9'],['a'..'f']]) !! n
byte2hex b = [nibble2Hex (shiftR b 4)] ++ [nibble2Hex (b .&. 15)]

encrypt string = result
  where
    bytes = map ord string
    keys = concat (repeat "ICE")
    keyBytes = map ord keys
    zipped = zip bytes keyBytes
    xored = map (\ (a,b) -> xor a b) zipped
    result = concat (map byte2hex xored)

main :: IO ()
main = do lines <- readFile "input.txt"
          putStrLn (encrypt lines)
