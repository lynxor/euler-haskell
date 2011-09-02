smallestDiv :: (Integral a) => a -> a
smallestDiv x = foldl (*) 1 (filterList [2 .. x])
                
filterList [] = []
filterList (x:xs) = if hasDivisible xs x 
                      then filterList xs
                    else x : filterList xs         

hasDivisible list item = not.null $ filter (\a -> a `mod` item == 0) list 

smallestDiv' :: (Integral a) => a -> a
smallestDiv' x = foldl foldFunc 1 ([x, x-1 .. 2]
  where foldFunc acc p = if hasDivisible [2 ..   
                                        
