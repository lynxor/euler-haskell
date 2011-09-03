primegen :: (Integral a) => [a]
primegen = gen [] 2
    where gen acc item = if isPrime acc item
                           then item: (gen (item:acc) (item+1))
                         else gen acc (item+1)  
          isPrime [] item = True 
          isPrime acc item = not $ any (\p -> item `mod` p == 0) acc

problem7 a = primegen !! (a-1)


