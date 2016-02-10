
--1. Find the last element of a list.
lastElem :: [a] -> a
lastElem [x] = x
lastElem (x:xs) = lastElem xs


--2. Find the last but one element of a list.
nextLastElem :: [a] -> a
nextLastElem [x, y] = x
nextLastElem (x:xs) = nextLastElem xs


--3. Find the K'th element of a list. The first element in the list is number 1.
findK' :: [a] -> Int -> a
findK' xs k = findK xs k 1
findK :: [a] -> Int -> Int -> a
findK (x:xs) k c  | match = x
                  | otherwise = findK xs k (c+1)
                  where match = k == c


--4 Find the number of elements of a list.
numElems :: [a] -> Int
numElems [] = 0
numElems (_:xs) = 1 + numElems xs




--5 Reverse a list.
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]



--6 Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
pal :: Eq e => [e] -> Bool
pal xs = ((rev xs) == xs)




--7 Flatten a nested list structure.




--8 Eliminate consecutive duplicates of list elements. Order should not be changed
dupes :: Eq e => [e] -> [e]
dupes [x] = [x] ++ []
dupes (x:xs) | match = dupes xs
             | otherwise = [x] ++ dupes xs
             where match = x == head xs
                



--9 Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq e => [e] -> [[e]]
pack [x] = [[x]] ++ []
pack (x:xs) | match = [[x]] ++ pack xs
            | otherwise = pack xs
              where match = x == head xs


