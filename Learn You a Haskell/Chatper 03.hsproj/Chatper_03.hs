
sayMe :: Int -> String
sayMe x | isSay = show x
        | otherwise = "Not between one a five"
        where isSay = 1 <= x && x <= 5
        


addVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVectors (a, b) (c, d) = (a+c, b+d)


third :: (a, b, c) -> c
third (_,_,z) = z


addList :: Num a => [a] -> a
addList [] = 0
addList (x:xs) = x + addList xs


firstLetter :: String -> String
firstLetter "" = "Whoops! Empty string."
firstLetter ys@(x:xs) = "The first letter of " ++ ys ++ " is " ++ [x] ++ "."


compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b  | a == b = EQ
                | a <= b = LT
                | otherwise = GT
                


bmi :: Float -> Float -> String
bmi w h | bmi <= skinny = "Underweight"
        | bmi <= normal = "Normal"
        | bmi <= fat    = "Fat"
        | otherwise     = "Whale"
        where bmi = w / h^2
              (skinny, normal, fat) = (18.5, 25.0, 30.0)
              

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
              where bmi weight height = weight / height^2
              
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w/h^2]
        

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."

initials' :: String -> String -> String
initials' firstname lastname = [f] ++ "." ++ [l] ++ "."
                            where (f:_) = firstname
                                  (l:_) = lastname
                                  



describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> " empty."
                                               [x] -> "a singleton."
                                               xs -> "a longer list."


describeList' :: [a] -> String
describeList' ls = "The list is " ++ d ls
                  where d []  = " empty."
                        d [x] = "a singleton."
                        d xs  = "a longer list."
                        
