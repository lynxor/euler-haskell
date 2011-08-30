import Data.List

findPrimes :: (Integral a) => a -> [a]
findPrimes a = foldl' (\acc num -> if isPrime acc num then num:acc else acc) [] [2 .. a]

findPrimesFor :: (Integral a) => a -> ([a],[a])
findPrimesFor num = foldl' foldFunc ([], []) [2 .. num]
                    where foldFunc (res, primes) test =  if isPrime primes test 
                                        then (divisible res num test, test:primes) 
                                      else (res, primes)
                          divisible res num test = if num `mod` test == 0 then test:res else res 

isPrime :: (Integral a) => [a] -> a -> Bool
isPrime [] _ = True
isPrime list num = null $ filter (\a -> num `mod` a == 0) list  

findPrimesFor' :: (Integral a) => a -> [a]
findPrimesFor' num = foldl' foldFunc [] [2 .. num]
  where foldFunc list test = if num `mod` test == 0 && isPrime list test 
                                   then test:list 
                                 else list 
                                      
erastothenes :: (Integral a) => a -> [a]
erastothenes a = 2 : sieve [3,5 .. a]
  where sieve [] = [] 
        sieve (x:xs) = x : (sieve $ xs `orderedMinus` [x*x, x*x+2*x..a])
        
orderedMinus (x:xs) (y:ys) = case (compare x y) of 
  LT -> x: orderedMinus xs (y:ys)
  EQ -> orderedMinus xs ys
  GT -> orderedMinus (x:xs) (ys)
orderedMinus xs _ = xs

probER :: (Integral a) => a -> [a]
probER a = filter (\p -> a `mod` p == 0) (erastothenes a)
                         
newMethod :: (Integral a) => a -> a           
newMethod  number = tester ( number `div` (head $ probER number))
  where tester p | p == 1 = number 
                 | otherwise = newMethod p

main = do number <- getLine 
          putStrLn $ show(head (findPrimesFor' (read number :: Integer)))