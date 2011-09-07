import Data.List

findGreatest :: (Integral a) => [a] -> a
findGreatest list = foldl foldFunc 0 (tails list)
  where foldFunc max sublist = if (prodFour sublist) > max 
                                 then prodFour sublist
                               else max
        prodFour l = product $ take 4 l

horiz :: String -> [[Int]]
horiz q = (map read).lines

vert :: String -> [[Int]]
vert q = transpose $ horiz q

diagr :: String -> Int -> [[Int]]
diagr q len = map (calcCol (horiz q)) (map (\l -> zip [1..] l) (horiz q))
  where calcCol horizq (ind, val) = map (\i -> (horizq !! ind) !! i) [ind, ind+1 .. len]
