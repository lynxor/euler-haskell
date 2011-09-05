import Data.Char

problem16 a = sum $ map digitToInt (show (2^a)) 