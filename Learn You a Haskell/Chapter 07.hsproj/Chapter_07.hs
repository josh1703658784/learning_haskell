--PAGE 129
import Shapes
data Vector a = Vector a a a deriving (Show)
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday           deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Person = Person { firstName :: String
                     , lastname  :: String 
                     , age       :: Int
                     , height    :: Float
                     , phoneNumber :: String
                     , flavor    :: String 
                     } deriving (Show, Eq, Read)
              
type Number = String
type Name = String
type Phonebook = [(Name, Number)]

phonebook :: Phonebook
phonebook = [("josh", "7246460485")
            ,("tesla", "8147044032")
            ]
            
inPhonebook :: Name -> Number -> Bool
inPhonebook name num = (name, num) `elem` phonebook

tellPerson :: Person -> String
tellPerson p = "I'm "++firstName p++" and I am "++show (age p)++" years old!"


vPlus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vPlus` (Vector x y z) = Vector (a+x) (b+y) (c+z)

vMult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vMult` (Vector x y z) = Vector (a*x) (b*y) (c*z)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector a b c) `dotProd` (Vector x y z) = (a*x)+(b*y)+(c*z)

--
--
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


