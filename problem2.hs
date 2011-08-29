fib :: Int -> Int 
fib 1 = 1
fib 2 = 2
fib x = fib (x-1) + fib (x-2)

problem2 = fibfunc 2 0 
           where fibfunc a acc = add a (fib a) acc
                 add a fibbed acc | (fibbed < 4000000) && (fibbed `mod` 2 == 0) = fibfunc (a+1) (acc+fibbed) 
                                  | (fibbed < 4000000) = fibfunc (a+1)(acc)
                                  | otherwise = acc