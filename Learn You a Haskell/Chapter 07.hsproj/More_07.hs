module More_07 
where

--data List a = Empty | Cons { listHead :: a, listTail :: List a}--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)infixr 5 :-:--data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
data List a = Empty | Cons { listHead :: a, listTail :: List a}

