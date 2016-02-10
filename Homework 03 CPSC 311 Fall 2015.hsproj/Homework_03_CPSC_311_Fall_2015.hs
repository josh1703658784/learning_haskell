module Homework where
import Criterion.Measurement
import qualified Data.Vector.Unboxed as V



--LINEAR SEARCH ARRAY
-- c is the index counter;
-- 'maybe's are useful for when something may or may not exist
linearSearchArray :: Ord a => a -> [a] -> Maybe Int
linearSearchArray f ys  | isListOrdered ys = linearSearch f 0 ys
                        | otherwise = error ("LIST IS NOT ORDERED")   --not important for linear

linearSearch :: Ord a => a -> Int -> [a] -> Maybe Int
linearSearch f c [] = Nothing                                         --the value could not be found
linearSearch f c (y:ys) | f == y = Just c                             --match: return index
                        | otherwise =  linearSearch f (c+1) ys        --look at next index
                      



--BINARY SEARCH ARRAY
--DOES A BINARY SEARCH FOR 'f' IN THE LIST 'ys'
-- l == low index; h == high index;
-- 'maybe's are useful for when something may or may not exist
binarySearchArray :: Ord a => a -> [a] -> Maybe Int
binarySearchArray f ys | isListOrdered ys = binarySearch f 0 (length ys) ys
                       | otherwise = error ("LIST IS NOT ORDERED")   --REALLY important for binary

binarySearch :: Ord a => a -> Int -> Int -> [a] -> Maybe Int
binarySearch f l h ys | err       = Nothing                             --the value could not be found
                      | f == v    = Just i                              --match: return index
                      | f < v     = binarySearch f l (i-1) ys     --search bottom half
                      | otherwise = binarySearch f (i+1) h ys     --search top half
                      where i     = (h+l) `quot` 2                --get halfway index
                            v     = ys !! i                       --get value at index
                            err   = l > h || i >= length ys       --is value not found
             

               

--IS LIST ORDERED
--CHECKS IF A LIST IS ORDERED
--this is *very* important before doing a binary search
isListOrdered :: Ord a => [a] -> Bool
isListOrdered [y] = True
isListOrdered (y:ys) | y <= head ys = isListOrdered ys
                     | otherwise = False



--VARIABLES
theList = [1, 5, 10, 12, 20, 26, 30]  --the list to search in
find = (26, 22)                       --the two values to search for


vector :: V.Vector Int
vector = V.fromList theList



--MAIN 
--just calls the different functions with different arguments 
--while timing how long it takes to do each search
main :: IO()
main = do 
          start <- getCPUTime
          result <- return (linearSearchArray (fst find) theList)
          end <- getCPUTime
          putStr ("Linear: " ++ show (fst find) ++ " @ " ++ show result ++ "; seconds: ")
          print ((end - start) / (10^12))
          
          start <- getCPUTime
          result <- return (linearSearchArray (snd find) theList)
          end <- getCPUTime
          putStr ("Linear: " ++ show (snd find) ++ " @ " ++ show result ++ "; seconds: ")
          print ((end - start) / (10^12))
          
          start <- getCPUTime
          result <- return (binarySearchArray (fst find) theList)
          end <- getCPUTime
          putStr ("Binary: " ++ show (fst find) ++ " @ " ++ show result ++ "; seconds: ")
          print $ ((end - start) / (10^12))
          
          start <- getCPUTime
          result <- return (binarySearchArray (snd find) theList)
          end <- getCPUTime
          putStr ("Binary: " ++ show (snd find) ++ " @ " ++ show result ++ "; seconds: ")
          print $ ((end - start) / (10^12))