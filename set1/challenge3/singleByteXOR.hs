import Data.Bits
import Data.Char
import Data.List
import Data.List.Split

ciphertext = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

allhex s = all isHexDigit s
combineHex [x,y] = shiftL x 4 + y

hex2intList h = result
  where
    hexPairs = chunksOf 2 (map digitToInt h)
    result = map combineHex hexPairs

nibble2Hex n = (concat [['0'..'9'],['a'..'f']]) !! n
byte2hex b = [nibble2Hex (shiftR b 4)] ++ [nibble2Hex (b .&. 15)]

count :: Eq a => a -> [a] -> Int
count e (l:ls) = (+) (if (==) e l then 1 else 0) (count e ls)
count e _ = 0

contains e (l:ls) = if (==) e l then True else contains e ls
contains e _ = False

uniq l = foldr (\ e l -> if (contains e l) then l else (e:l)) [] l

mostFrequentBytes l = result
  where
    counts = map (\ e -> count e l) l
    pairs = zip l counts
    result = uniq (sortBy (\ (a,b) (c,d) -> compare d b) pairs)

decrypt l k = result
  where
    decrypted = map (\ e -> xor e k) l
    result = map chr decrypted

tryAllDecryptions l = result
  where
    frequentBytes = fst (unzip (mostFrequentBytes l))
    keys = map (\ e -> xor e (ord ' ')) frequentBytes
    decryptions = map (\ k -> decrypt l k) keys
    result = zip decryptions keys

main :: IO()
main = putStr ((show (tryAllDecryptions (hex2intList ciphertext))) ++ "\n")
