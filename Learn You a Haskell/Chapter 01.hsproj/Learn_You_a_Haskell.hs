doubleMe :: Num n => n -> n
doubleMe x = x + x

doubleUs :: Num n => n -> n -> n
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Ord n => Num n => n -> n
doubleSmallNumber x | small = doubleMe x
                    | otherwise = x
                     where small = (x <= 100)
                    

doubleSmallNumber' :: Ord n => Num n => n -> n
doubleSmallNumber' x = 1 + doubleSmallNumber x

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]



length' xs = sum [ 1 | _ <- xs]


removeLowerCase xs = [ x | x <- xs, x `elem` ['A'..'Z']]


removeNestedOdds xxs = [[ x | x <- xs, even x ] | xs <- xxs ]


findTriangle = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a+b+c==24, a^2+b^2==c^2]

--p = a + b + c
-- l <= 10