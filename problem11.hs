import Data.List

findGreatest :: (Integral a) => [a] -> a
findGreatest list = foldl foldFunc 0 (tails list)
  where foldFunc max sublist = if (prodFour sublist) > max 
                                 then prodFour sublist
                               else max
        prodFour l = product $ take 4 l

horiz :: String -> [[Int]]
horiz q = map ((map read).words) (lines q)

vert :: String -> [[Int]]
vert q = transpose $ horiz q

diag :: [[Int]] -> Int -> [[Int]]
diag horizq rlen  = map (mapZipped horizq) (tails [0 .. (rlen-1)])
  where mapZipped horizq indexes = map (\(ind, list) -> list !! ind) (zip indexes horizq)

problem11 :: String -> Int -> Int
problem11 q dim = foldl foldFunc 0 allLines
   where allLines = (horiz q) ++ (vert q) ++ diagr ++ diagl 
         diagr = (diag (horiz q) dim) ++ (diag (vert q) dim)
         diagl = (diag (map reverse (horiz q)) dim) ++ (diag (map reverse (vert q)) dim) 
         foldFunc max item = if (findGreatest item) > max 
                               then findGreatest item
                             else max

main = do 
  content <- readFile "problem11_number"
  putStrLn $ show (problem11 content 20)