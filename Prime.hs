module Prime
( erastothenes
, erastothenes'
, largestPrime
, orderedMinus
, smallestPrime
) where

erastothenes :: (Integral a) => a -> [a]
erastothenes a = 2 : sieve [3,5 .. a]
  where sieve [] = [] 
        sieve (x:xs) = x : (sieve $ xs `orderedMinus` [x*x, x*x+2*x..a])
        
-- Infinite version
erastothenes' :: (Integral a) => [a]
erastothenes' = 2 : sieve [3,5 .. ]
  where sieve [] = [] 
        sieve (x:xs) = x : (sieve $ xs `orderedMinus` [x*x, x*x+2*x..])



orderedMinus (x:xs) (y:ys) = case (compare x y) of 
  LT -> x: orderedMinus xs (y:ys)
  EQ -> orderedMinus xs ys
  GT -> orderedMinus (x:xs) (ys)
orderedMinus xs _ = xs

probER :: (Integral a) => a -> [a]
probER a = filter (\p -> a `mod` p == 0) (erastothenes a)
                         
-- Call like this: "largestPrime 13195"
largestPrime :: (Integral a) => a -> a           
largestPrime  number = tester ( number `div` (head $ probER number))
  where tester p | p == 1 = number 
                 | otherwise = largestPrime p

smallestPrime :: (Integral a) => a -> a
smallestPrime number = head $ probER number