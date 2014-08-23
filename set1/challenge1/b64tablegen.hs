tuple2line :: (Char,Integer) -> [Char]
tuple2line (c,i) = "b64i " ++ show i ++ " = '" ++ [c] ++ "'\n"

main :: IO ()
main = do putStr result
  where
    alphabet = concat [['A'..'Z'],['a'..'z'],['0'..'9'],['+'],['/']]
    strings = map tuple2line (zip alphabet [0..])
    result = concat strings
