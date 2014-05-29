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

-- 23 TODO: can the result be repeated?
rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = do
	return []
rndSelect xs n = do
	gen    <- getStdGen
	value  <- xs !! (randomRs (0, (length xs)-1) gen !! 0)
	xss    <- rndSelect (removeItem value xs) (n-1)
	return $  [value] ++ xss


-- 24
-- diffSelect :: Int -> Int -> IO Int
-- diffSelect n m = do
-- 	stdGen <- getStdGen
-- 	return $ take n 


-- utils
removeItem _ [] = []
removeItem x (y:ys)
	| x == y = removeItem x ys
	| otherwise = y : removeItem x ys




