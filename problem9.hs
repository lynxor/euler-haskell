import Data.Maybe
import Data.List

soss :: (Integral a, Floating b) => a -> a -> b
soss a b = sqrt $ sumOfSquares (fromIntegral a) (fromIntegral b) 
  where sumOfSquares a b = a^2 + b^2

problem9 :: (Integral a) => a         
problem9 = gen [1 .. 1000]
  where gen (x:xs) = genTest x $ find (findFunc x) xs
        genTest a m 
             | isJust m = a * (fromJust m) * (1000-(a+ (fromJust m))) 
             | isNothing m = gen [(a+1) .. 1000]  

findFunc :: (Integral a) => a -> a -> Bool
findFunc a b = (fromIntegral a) + (fromIntegral b) + (soss a b) == 1000
        