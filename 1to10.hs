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



