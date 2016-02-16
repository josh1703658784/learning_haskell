import Shapes

data Person = Person { firstName :: String
                     , lastname  :: String 
                     , age       :: Int
                     , height    :: Float
                     , phoneNumber :: String
                     , flavor    :: String 
                     } deriving (Show)
              

--PAGE 117






--firstName :: Person -> String
--firstName (Person fn _ _ _ _ _) = fn
--
--lastName :: Person -> String
--lastName (Person _ ln _ _ _ _) = ln
--
--age :: Person -> Int
--age (Person _ _ a _ _ _) = a
--
--height :: Person -> Float
--height (Person _ _ _ h _ _) = h
--
--phoneNumber :: Person -> String
--phoneNumber (Person _ _ _ _ pn _) = pn
--
--flavor :: Person -> String
--flavor (Person _ _ _ _ _ i) = i