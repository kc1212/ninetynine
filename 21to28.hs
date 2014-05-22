
-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt a xs 1 = a : xs
insertAt a (x:xs) n = x : insertAt a xs (n-1)

-- 22
range :: Int -> Int -> [Int]
range a b
	| a < b = a : range (a+1) b
	| a == b = [a]
	| otherwise = error "a is larger than b."

-- 23






