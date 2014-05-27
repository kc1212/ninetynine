import System.Random

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
rndSelect xs n = do
	stdGen <- getStdGen
	return $ take n [xs !! x | x <- randomRs (0,(length xs) - 1) $ stdGen]





