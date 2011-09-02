import Data.List

problem7 a = (gen 2) !! (a-1) 

gen :: (Integral a) => a -> [a]
gen num = if prime num then num : gen (num + 1) else gen (num+1)  


prime :: (Integral a) => a -> Bool
prime num = (smallestPrime num) == num

-- gen' :: (Integral a) => [a] -> a -> [a]
-- gen' primes num = if prime' primes num 
--                     then num : gen' (num:primes) (num+1) 
--                   else gen' primes (num+1)

-- prime' :: (Integral a) => [a] -> a -> Bool
-- prime' primes num = not $ any (\p -> num `mod` p == 0) primes

-- probER :: (Integral a) => a -> [a]
-- probER a = filter (\p -> a `mod` p == 0) (gen' [] 2)

-- smallestPrime :: (Integral a) => a -> a
-- smallestPrime number = head $ probER number