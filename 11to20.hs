
import Data.List (group)

-- 11
data RunLength a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [RunLength a]
encodeModified [] = []
encodeModified xs = map encodeModifiedHelper (group xs)

encodeModifiedHelper :: [a] -> RunLength a
encodeModifiedHelper xs
	| length xs == 1 = Single (head xs)
	| length xs > 1 = Multiple (length xs) (head xs)
	| otherwise = error "List has length of zero"

-- 12
decodeModified :: Eq a => [RunLength a] -> [a]
decodeModified [] = []
decodeModified xs = concat $ map decodeModifiedHelper xs

decodeModifiedHelper :: RunLength a -> [a]
decodeModifiedHelper (Single x) = [x]
decodeModifiedHelper (Multiple i x) = take i $ cycle [x]

-- 13
-- ?

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ cycle [x]) ++ (repli xs n)

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n
	| rem (length xs) n == 0 = dropEvery (init xs) n
	| otherwise = (dropEvery (init xs) n) ++ [last xs]

-- 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split (x:xs) n
	| n > 0 = (x : fst (split xs $ n-1), snd (split xs $ n-1))
	| otherwise = (fst (split xs $ n-1), x : snd (split xs $ n-1))

-- 18
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs a b = take (b-a+1) step1
	where
		step1 = lastN ((length xs)-a+1) xs

-- 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n = rotate (xs ++ [x]) $ n-1

-- 20
-- here we use 0 as start of index, unlike the question
removeAt :: Int -> [a] -> (a,[a])
removeAt _ [] = error "Empty list"
removeAt 0 (x:xs) = (x, xs)
removeAt n (x:xs) = (one, x:two)
	where
		(one, two) = removeAt (n-1) (xs)




