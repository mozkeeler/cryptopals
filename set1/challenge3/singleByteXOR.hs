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

count (l:ls) e = (+) (if (==) l e then 1 else 0) (count ls e)
count _ e = 0

contains (l:ls) e = if (==) l e then True else False
contains _ e = False

uniq l = foldr (\ e l -> if (contains l e) then l else (e:l)) [] l

occurrences l = result
  where
    sorted = sort l
    counts = map (\ e -> count l e) sorted
    unique = uniq (zip sorted counts)
    max = last (sortBy (\ (a,b) (c,d) -> compare b d) unique)
    result = max

main :: IO()
main = putStr ((show (occurrences (hex2intList ciphertext))) ++ "\n")
