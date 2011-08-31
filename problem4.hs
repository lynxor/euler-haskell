-- Call like this : problem4 999, or problem4 99
problem4 a = head $ dropWhile (dropFunc [a,a-1 .. a-(a `div` 10) ]) (findPalindromes)
  where dropFunc options item = null $ dropWhile (\p -> item `mod` p /= 0 || not (elem (item `div` p)  options)) options 
        findPalindromes = filter (palindrome.show) [a*a, a*a -1 .. ]
        palindrome x = reverse x == x

