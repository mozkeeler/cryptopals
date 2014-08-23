import Data.Bits
import Data.Char
import Data.List.Split

allhex s = all isHexDigit s

b64i i = alphabet !! i
  where
    alphabet = concat [['A'..'Z'],['a'..'z'],['0'..'9'],['+'],['/']]

regroup (x:[]) = regroup (x:0:0:[])
regroup (x:y:[]) = regroup (x:y:0:[])
regroup (x:y:z:[]) = (i,j,k,l)
  where
    i = shiftR x 2
    j = shiftL x 4 .&. 48 + shiftR y 4
    k = shiftL (y .&. 15) 2 + shiftR z 6
    l = z .&. 63

tuple2base64 :: (Int,Int,Int,Int) -> [Char]
tuple2base64 (i,j,k,l) = (b64i i) : (b64i j) : (b64i k) : [b64i l]

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
          if allhex l && mod (length l) 2 == 0 then 
            putStr ((hex2base64 l) ++ "\n")
          else
            putStr ("Input not hex or not even-length: '" ++ l ++ "'\n")
