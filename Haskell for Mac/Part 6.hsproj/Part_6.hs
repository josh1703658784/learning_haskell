type Point = (Float, Float)
type Line  = (Point, Point)

type FancyLine = (Point, Point, LineStyle)

data LineStyle
  = Solid
  | Dashed
  | Dotted
  

data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Enum, Show, Eq)
  


isWeekday :: Day -> Bool
--isWeekday Saturday = False
--isWeekday Sunday   = False
--isWeekday _        = True
--
--isWeekday d = case d of
--            Sunday   -> False
--            Saturday -> False
--            _        -> True
--
isWeekday d = not $ d `elem` [Saturday,Sunday]

