import Prime

elimDiv :: Int -> Int
elimDiv a = foldl foldFunc 1 [2 .. a]
        where foldFunc acc item = if (acc `mod` item == 0)
                                    then acc
                                  else (Prime.smallestPrime item)*acc

