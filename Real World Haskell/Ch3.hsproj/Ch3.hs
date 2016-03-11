import Data.List

type CustomerID = Int
type ReviewBody = String
type CardHolder = String
type CardNumber = String
type Address    = [String]
type CashOnDelivery = String
type Invoice        = String

--type BookRecord = (BookInfo, BookReview)
--pg 60
data BookInfo = Book {
                identifier :: Int
                ,title      :: String
                ,authors    :: [String]
                } deriving (Show)

data BookReview = BookReview {
                bookInfo   :: BookInfo
                ,customerId :: CustomerID
                ,reviewBody :: ReviewBody
                } deriving (Show)
 
 
data Customer = Customer {
          --   customerId      :: CustomerID
              customerName    :: String
              ,customerAddress :: Address
              } deriving (Show)

--
data BillingInfo = CreditCard CardNumber CardHolder Address
                | CashOnDelivery
                | Invoice CustomerID
                  deriving (Show)
                  



--data Tree a = Node a (Tree a) (Tree a)
--            | Empty
--            deriving (Show)
            
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving (Show)
            

--
data List a = Empty | Cons a (List a) deriving (Show, Eq)
--              
--
fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs) 
fromList [] = Empty


toList :: List a -> [a]
toList Empty = []
toList (Cons x xs) = x : toList xs


--Q1
length' :: [a] -> Int
length' xs = sum . map (\x -> 1) $ xs

--Q2
--add type signature to above

--Q3
--listMean :: [a] -> Integral
listMean xs = fromIntegral (sum xs) / fromIntegral (length' xs)

--q4
makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ reverse xs

--q5
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs


--q6
sortLists :: Ord a => [[a]] -> [[a]]
sortLists (xs) = sortBy (des) sortSub
                where sortSub = map (\x -> sortBy (des) x) xs


--sortLt :: [a] -> [a]
des a b = a `compare` b

--q7/8
--intersperse :: a -> [[a]] -> [a]


--q9
