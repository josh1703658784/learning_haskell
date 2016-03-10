type CustomerID = Int
type ReviewBody = String
type CardHolder = String
type CardNumber = String
type Address    = [String]
type CashOnDelivery = String
type Invoice        = String

--type BookRecord = (BookInfo, BookReview)

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
--            


data List a = Empty | Cons a (List a) deriving (Show, Eq)
              

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs) 
fromList [] = Empty


toList :: List a -> [a]
toList Empty = []
toList (Cons x xs) = x : toList xs

