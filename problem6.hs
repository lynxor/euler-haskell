sumOfSquares :: (Integral a) => [a] -> a
sumOfSquares = sum.map (^2)

squareOfSum :: (Integral a) => [a] -> a
squareOfSum = (^2).sum

-- Difference between the square of the sum and the sum of squares
problem5 :: (Integral a) => a -> a
problem5 a = squareOfSum [1 .. a] - sumOfSquares [1 .. a]