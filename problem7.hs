import Data.List
import qualified Prime as Prime

problem7 a = genprimes !! a-1 

genprimes :: (Integral a) => [a]
genprimes = gen [] 2

gen :: (Integral a) => [a] -> a -> [a]
gen acc num = if prime acc num then num : (gen (num:acc) (num+1)) else gen acc (num+1)   

prime :: (Integral a) => a -> Bool
prime num = Prime.smallestPrime num /= num