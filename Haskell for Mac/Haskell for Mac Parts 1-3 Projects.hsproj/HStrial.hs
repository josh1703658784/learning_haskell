
module Examples where
import Data.Char 
import Prelude hiding (enumFromTo)

--FIBONACCI LIST
fib' :: [Int]
fib' = 1 : 1 : zipWith (+) fib' (tail fib')


--FIBONACCI
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-2) + fib(n-1)



--FACTORIAL
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)


--FIZZ BUZZ
fizzBuzz :: [Int] -> [String]
fizzBuzz [] = []
fizzBuzz (x:xs) | isThree && isFive = "fizzbuzz" : fizzBuzz xs
                | isThree = "fizz" : fizzBuzz xs
                | isFive = "buzz" : fizzBuzz xs
                | otherwise = show x : fizzBuzz xs
                where isThree = x `mod` 3 == 0
                      isFive = x `mod` 5 == 0
                


--SUBTRACT TWO SETS
subtractSets :: [Int] -> [Int] -> [Int]
subtractSets xs ys = xs ++ ys

--REMOVE ALL DUPLICATES
removeAllDuplicates :: [Int] -> [Int]
removeAllDuplicates [] = []
removeAllDuplicates (x:xs)  | dupeFound = removeAllDuplicates xs
                            | otherwise = [x] ++ removeAllDuplicates xs
                              where dupeFound = findDuplicate x xs
                              
--removeXFromList :: [Int] -> [Int]
--removeXFromList [] = []
--removeXFromList x (y:ys)  | (x==y) = removeXFromList x ys
--                          | otherwise = 


--UNION TWO SETS
unionSets :: [Int] -> [Int] -> [Int]
unionSets xs ys = removeDupes (xs ++ ys)
--REMOVE DUPLICATES
removeDupes :: [Int] -> [Int]
removeDupes [] = []
removeDupes (x:xs)  | findDuplicate x xs = removeDupes xs
                    | otherwise = [x] ++ removeDupes xs
--                      where dupeFound = findDuplicate x xs
--FIND DUPLICATE
findDuplicate :: Int -> [Int] -> Bool
findDuplicate x [] = False
findDuplicate x (y:ys)  | x == y = True
                        | otherwise = findDuplicate x ys








--DEDUCT MONEY FROM AN ACCOUNT; THROW ERROR IF TOO MUCH DEDUCTED;
deductFromAccount :: Int -> [Int] -> Int
deductFromAccount x [] = x
deductFromAccount x (y:ys) | (x-y) < 0 = error ("ERROR: You have $" ++ show x ++ " in your account but are trying to deduct $" ++ show y)
                           | otherwise = deductFromAccount (x-y) ys



--REVERSE A LIST
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]
--reverse' (xs:x) = reverse' xs : x



--CONCAT TWO LISTS
--(++) :: [a] -> [a] -> [a]
--[] Examples.(++) ys = ys
--(x:xs) Examples.(++) ys = x : (xs ++ ys)


--FIND MINIMUM OF A LIST
minList :: [Int] -> Int
minList [] = maxBound
minList (x:xs) = x `min` minList xs





--FIND SUM OF LIST
listSum :: Num a => [a] -> a
listSum [] = 0
listSum (x:xs) = x + listSum xs



--FIND PRODUCT OF THE PASSED LIST
product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * Examples.product xs
  
--REMOVE NON-NUMERALS FROM STRING
extractDigits :: String -> String
extractDigits [] = []
extractDigits (x:xs) | isDigit = x : extractDigits xs
                     | otherwise = extractDigits xs
                     where isDigit = elem x ['0'..'9']


--CONVERT STRING TO ALL UPPER CASE
allToUpper :: String -> String
allToUpper [] = []
allToUpper (x:xs) = toUpper x : allToUpper xs --toUpper is from Data.Char



--FIND SQUARED VALUE OF EVERY ELEMENT IN A PASSED LIST
allSquares :: [Int] -> [Int]
allSquares [] = []
allSquares (x:xs) = x * x : allSquares xs



--RETURN ALL POSSIBLE SUFFIXES OF A PASSED STRING
suffixes :: String -> [String]
suffixes "" = []
suffixes xs = xs : suffixes (tail xs)



--SORT A TUPLET OF SIZE TWO
sort2 :: Ord a => a -> a -> (a, a)
sort2 x y | x < y = (x, y)
          | otherwise = (y , x)



--DO TWO TUPLETS CONTAIN THE SAME VALUES
almostEqual :: Eq a => (a, a) -> (a, a) -> Bool
almostEqual (x, y) (w, z) = (x,y) == (w,z) || (y, x) == (w, x)



--IS THE PASSED VALUE LOWERCASE
isLower :: Char -> Bool
isLower x = elem x ['a'..'z']



--REMOVE FIRST LETTER FROM WORD AND APPEND TO END OF WORD
mangle :: String -> String
mangle xs = tail xs ++ take 1 xs 



--REPEAT PASSED VALUE N TIMES IN AN ARRAY
repeatN :: Int -> a -> [a]
repeatN 0 l = []
repeatN n l = l : repeatN(n-1) l



-- MANUALLY DIVIDE A NUMBER
  --FIND THE MULTIPLES OF THE PASSED NUMBER
findMultiples :: Int -> Int -> [Int]
findMultiples c 0 = []
findMultiples c y | isClean = y : findMultiples c (y-1)
                  | otherwise = findMultiples c (y-1)
                  where isClean = (c `mod` y == 0)
    --"MANUALLY" DIVIDE
divide :: Int -> Int -> Int
divide x y  | (x `elem` findMultiples y y) = y `div` x
            | otherwise = 0



--RETURN THE HEAD OF THE LIST
head' :: [a] -> a
head' (x:_) = x



--NATURAL SUM OF PASSED NUMBER
natSum :: Int -> Int
natSum n  | n > 0 = n + natSum(n - 1)
          | otherwise = 0
  


--FIND THE AVERAGE OF TWO NUMBERS
average :: Float -> Float -> Float
average x y = (x + y) / 2.0



--FIND THE MAX OF TWO PASSED NUMBERS
max' :: Ord a => a -> a -> a
max' x y | x > y = x
         | otherwise = y
         


--FIND THE AREA OF THE CIRCLE
circlearea :: Float -> Float
circlearea d = pi * r * r
              where r = d/2
              


--ADD AND MULTIPLY TWO PASSED NUMBERS; RETURN RESULT AS A TUPLE;
addMult :: Num a => a -> a -> (a, a)
addMult x y = (x+y, x*y)



--MAKING YOUR OWN TYPES...ETC
type Point = (Int, Int)
origin :: Point
origin = (0, 0)



--3. RECURSIVE FACTORIAL
fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n-1)


--4. ENUMFROMTO
enumFromTo :: Int -> Int -> [Int]
enumFromTo m n | isEqual = [m]
               | otherwise = m:enumFromTo (m+1) n
               where isEqual = (m==n)


----5. Count number of odds
countOdds :: [Int] -> Int
countOdds [] = (1)
countOdds (x:xs) | isOdd = (1+):countOdds xs
                 | otherwise = countOdds xs
                 where isOdd= odd x
                 


--6. Removes odd
removeOdds :: [Int] -> [Int]
removeOdds [] = []
removeOdds (x:xs) | isOdd = removeOdds xs
                 | otherwise = x:removeOdds xs
                 where isOdd= odd x