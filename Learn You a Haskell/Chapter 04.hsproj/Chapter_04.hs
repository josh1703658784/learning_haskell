maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
maximum' (x:xs) = x `max'` (maximum' xs)



max' :: (Ord a) => a -> a -> a
max' x y | x < y = y
         | otherwise = x
        


replicate' :: Int -> a -> [a]
replicate' 0 v = []
replicate' r v = [v] ++ replicate' (r-1) v


take' :: Int -> [a] -> [a]
take' n (x:xs) | n <= 0 = []
               | otherwise = x : take' (n-1) xs
               


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]
repeat' x = x : repeat' x


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip xs ys



elem' :: Eq a => [a] -> a -> Bool
elem' [] f = False
elem' (x:xs) f | found = True
               | otherwise = elem' xs f
               where found = x == f
               



quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort lower ++ [p] ++ quicksort upper
                  where lower = [l | l <- xs, l <= p]
                        upper = [u | u <- xs, u > p]