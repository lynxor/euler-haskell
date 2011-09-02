import Prime

main = do
  input <- getLine
  putStrLn (show (largestPrime (read input)))