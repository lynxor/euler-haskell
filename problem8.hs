import Data.List
import Data.Char
import System.IO

greatestProduct :: String -> Int
greatestProduct num = foldl foldFunc 0 (tails (map digitToInt num))
  where foldFunc max str = if product (take 5 str) > max 
                             then product (take 5 str) 
                           else max

main = do 
  contents <- readFile "problem8_number"     --reads that crazy number from file
  putStrLn $ show (greatestProduct contents)