import Data.List
import Data.Char

greatestProduct :: String -> Int
greatestProduct num = foldl foldFunc 0 (tails (map digitToInt num))
  where foldFunc max str = if product (take 4 str) > max 
                             then product (take 4 str) 
                           else max
horiz :: String -> [[Int]]
horiz q = map ((map read).words) (lines q)

vert q = transpose $ horiz q

diagr :: [[Int]] -> Int -> [[Int]]
diagr horizq rlen  = map (mapZipped horizq) (tails [0 .. (rlen-1)])
                     
mapZipped :: [[Int]] -> [Int] -> [Int]
mapZipped horizq indexes = map mapToIndex (zip indexes horizq)

mapToIndex :: (Int, [Int]) -> Int
mapToIndex (ind, list) = list !! ind

problem11 q = (horiz q) 

