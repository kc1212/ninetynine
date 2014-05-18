
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
-- dropEvery :: [a] -> Int -> [a]




