import Data.Char

problem20 x = sum $ map digitToInt (show (product [1 .. x]))
