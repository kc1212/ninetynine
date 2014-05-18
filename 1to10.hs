-- 1
myLast :: [a] -> a
myLast [] = error "The list is empty!"
myLast [a] = a
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "The list is empty!"
myButLast [a] = error "The list is too short!"
myButLast [a,_] = a
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Element does not exist!"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = (+1) $ myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs =
	h == l && isPalindrome m
	where
		h = head xs
		l = last xs
		m = init $ tail xs
-- or just check the reverse

-- 7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:xs)
	| x == head xs = compress xs
	| otherwise = x : compress xs

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [a] = [[a]]
pack (x:xs)
	| x == head xs = ( x:(head p) ) : tail p
	| otherwise = [x] : p
	where
		p = pack xs

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

