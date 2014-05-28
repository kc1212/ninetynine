
-- 31
isPrime :: Int -> Bool
isPrime x = all ((0 /=) . (rem x)) [2..x-1]

