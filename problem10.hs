import Prime
import Data.List

problem10 x = sum.takeWhile (<x) $ (erastothenes 9999999)

-- don't really get why infinite version is slower ???
-- problem10 x = sum.takeWhile (<x) $ erastothenes'

main = do putStrLn $ show ( problem10 2000000)