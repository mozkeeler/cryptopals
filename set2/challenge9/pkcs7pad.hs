import Data.Char
import Data.Char
import Data.List
import Data.List.Split

pkcs7pad len bytes = result
  where
    chunks = chunksOf len bytes
    lst = last chunks
    needed = len - (length lst)
    padding = repeat needed
    paddedLast = concat [lst,(take needed padding)]
    chunksMinusLast = take (length chunks - 1) chunks
    result = concat [chunksMinusLast,[paddedLast]]

main :: IO()
main = putStrLn result
  where
    bytes = pkcs7pad 20 (map ord "YELLOW SUBMARINE")
    result = show bytes
