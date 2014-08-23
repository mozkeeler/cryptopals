import Data.Bits
import Data.Char
import Data.List.Split

allhex s = all isHexDigit s

regroup (x:[]) = regroup (x:0:0:[])
regroup (x:y:[]) = regroup (x:y:0:[])
regroup (x:y:z:[]) = (i,j,k,l)
  where
    i = shiftR x 2
    j = shiftL x 4 .&. 48 + shiftR y 4
    k = shiftL (y .&. 15) 2 + shiftR z 6
    l = z .&. 63

tuple2base64 :: (Int,Int,Int,Int) -> [Char]
tuple2base64 (i,j,k,l) = (b64i i) ++ (b64i j) ++ (b64i k) ++ (b64i l)

combineHex [x,y] = shiftL x 4 + y

hex2base64 s = result
  where
    hexPairs = chunksOf 2 (map digitToInt s)
    hex = map combineHex hexPairs
    padding = mod (3 - mod (length hex) 3) 3
    chunks = chunksOf 3 hex
    tuples = map regroup chunks
    resultWithAPads = concat (map tuple2base64 tuples)
    resultWithNoPad = reverse . drop padding . reverse $ resultWithAPads
    result = resultWithNoPad ++ concat ["=" | r <- [1..padding]]

evenLength s = mod (length s) 2 == 0

main :: IO ()
main = do l <- getLine
          if allhex l then 
            putStr ((hex2base64 l) ++ "\n")
          else
            putStr ("Input not hex or not even-length: '" ++ l ++ "'\n")

b64i 0 = "A"
b64i 1 = "B"
b64i 2 = "C"
b64i 3 = "D"
b64i 4 = "E"
b64i 5 = "F"
b64i 6 = "G"
b64i 7 = "H"
b64i 8 = "I"
b64i 9 = "J"
b64i 10 = "K"
b64i 11 = "L"
b64i 12 = "M"
b64i 13 = "N"
b64i 14 = "O"
b64i 15 = "P"
b64i 16 = "Q"
b64i 17 = "R"
b64i 18 = "S"
b64i 19 = "T"
b64i 20 = "U"
b64i 21 = "V"
b64i 22 = "W"
b64i 23 = "X"
b64i 24 = "Y"
b64i 25 = "Z"
b64i 26 = "a"
b64i 27 = "b"
b64i 28 = "c"
b64i 29 = "d"
b64i 30 = "e"
b64i 31 = "f"
b64i 32 = "g"
b64i 33 = "h"
b64i 34 = "i"
b64i 35 = "j"
b64i 36 = "k"
b64i 37 = "l"
b64i 38 = "m"
b64i 39 = "n"
b64i 40 = "o"
b64i 41 = "p"
b64i 42 = "q"
b64i 43 = "r"
b64i 44 = "s"
b64i 45 = "t"
b64i 46 = "u"
b64i 47 = "v"
b64i 48 = "w"
b64i 49 = "x"
b64i 50 = "y"
b64i 51 = "z"
b64i 52 = "0"
b64i 53 = "1"
b64i 54 = "2"
b64i 55 = "3"
b64i 56 = "4"
b64i 57 = "5"
b64i 58 = "6"
b64i 59 = "7"
b64i 60 = "8"
b64i 61 = "9"
b64i 62 = "+"
b64i 63 = "/"
