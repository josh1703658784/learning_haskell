multThree :: Int -> Int -> Int -> Int
multThree x y z = x*y*z

--type is whatever multThree returns
multTwoWithNine = multThree 9

--type stays the same as 'compare' because that's what returned
compareWithHundred = compare 100


--to curry infix functions just surround by parenthesis
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])



applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

applyThrice :: (a -> a) -> a -> a
applyThrice f x = f (f (f x))


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys



flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | (f x == True) = x : filter' f xs
                 | otherwise = filter' f xs
                 


--QUICKSORT WITH FILTERS
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort lower ++ [p] ++ quicksort upper
                  where lower = filter (<p) xs
                        upper = filter (>=p) xs
                        

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
                  where p x = x `mod` 3829 == 0
                  
sumDifference :: (Num a) => a -> a -> (a,a)
sumDifference a b = (a+b, a-b)


--SUM ODD SQUARES < 10,000
--sumOddSquares = sum  (takeWhile (<10000) (filter (odd) (map (^2) [1..])))
sumOddSquares = sum . takeWhile (<10000) . filter (odd) $ map (^2) [1..]


--COLLATZ CHAIN
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n | odd n = n : collatz (odd_calc)
          | even n =  n : collatz (even_calc)
          where even_calc = (n `div` 2)
                odd_calc = (n * 3 + 1)


collatzLength :: Int
collatzLength = length(filter (isLong) (map collatz [1..100]))
                where isLong x = length x > 15
                
collatzLength' :: Int
collatzLength' = length(filter (\xs -> length xs > 15) (map collatz [1..100]))


 
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z


sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
--variables implicitly passed with currying


map' :: (a -> a) -> [a] -> [a]
map' f xs = foldr (\x acc -> (f x):acc) [] xs



elem' :: (Eq e) => e -> [e] -> Bool
elem' x ys = foldr (\y acc -> (x==y)||acc) False ys


maximum' :: (Ord o) => [o] -> o
maximum' xs = foldl1 max xs


reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []


product' :: (Num a) => [a] -> a
product' = foldl1 (*)



filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x:acc else acc) []


last' :: [a] -> a
last' = foldl1 (\_ x -> x)


sqrtSums :: Int
--sqrtSums = length (takeWhile (<1000) (scanl (+) 0 (map sqrt [1..])))
sqrtSums = length $ takeWhile (<1000) $ scanl (+) 0 $ map sqrt [1..]

--sqrt (fromIntegral x)

negate_list :: Num a => [[a]] -> [a]
negate_list = map (negate . sum . tail)



sumOddSquares' = sum . filter (odd) . takeWhile (<10000) $  map (^2) [1..]
--sumOddSquares' =  filter odd $ map (^2) [1..]
