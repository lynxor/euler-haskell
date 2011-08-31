problem4 a = head $ dropWhile (dropFunc [a,a-1 .. a-(a `div` 10) ]) (findPalindromes a)
  where dropFunc options item = null $ dropWhile (\p -> item `mod` p /= 0 || not (elem (item `div` p)  options)) options 
        

findPalindromes :: (Integral a) => a -> [a]
findPalindromes x = filter (palindrome.show) [x*x, x*x - 1 .. ]   

        
palindrome :: String -> Bool
palindrome a = reverse a == a