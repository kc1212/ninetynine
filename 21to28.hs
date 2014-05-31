import System.Random (randomRIO)

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
rndSelect :: Eq a => [a] -> Int -> IO [a]
rndSelect _ 0  = return []
rndSelect [] _ = return []
rndSelect xs n = do
	i <- randomInt (length xs)
	new <- rndSelect (removeItem (xs!!i) xs) (n-1)
	return $ (xs!!i) : new

-- 24
-- diffSelect :: Int -> Int -> IO Int
-- diffSelect n m = do
-- 	stdGen <- getStdGen
-- 	return $ take n 


-- utils
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
	| x == y = removeItem x ys
	| otherwise = y : removeItem x ys

randomInt :: Int -> IO Int
randomInt x = do
	c <- randomRIO (0,x-1) -- note the -1
	return c



