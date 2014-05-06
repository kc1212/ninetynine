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
-- elementAt :: [a] -> a
