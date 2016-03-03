module Fractals where
  -- needed to display the picture in the playground
import Codec.Picture
  -- our line graphics programming interface
import LineGraphics

house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]
door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]


rotateLine :: Float -> Line -> Line
rotateLine n ((x1,y1),(x2,y2)) = ((x1, y1), (x1+x', y1+y'))
                 where x0 = x2 - x1
                       y0 = y2 - y1
                       x' = x0 * cos n - y0 * sin n
                       y' = x0 * sin n + y0 * cos n
                                  

scaleLine :: Float -> Line -> Line
scaleLine n ((x1,y1),(x2,y2)) = ((x1,y1),(x'+x1,y'+y1))
            where x0 = x2 - x1
                  y0 = y2 - y1
                  x' = n * x0
                  y' = n * y0

fade :: Colour -> Colour
fade (r,g,b,o) = (r,g,b,o-1)

spiralRays :: Float -> Float -> Int -> Colour -> Line -> Picture
spiralRays angle scaleFactor n colour line
  = spiralRays' n colour line
  where
    spiralRays' n colour line@(p1, p2)
      | n <= 0 = []
      | otherwise = (colour, [p1, p2]) : spiralRays' (n-1) (changeColour newColour) (changeLine newLine)
      where
        newColour = fade colour
        newLine   = scaleLine scaleFactor (rotateLine angle line)
        

spiral :: Float -> Float -> Int -> Line -> Picture
spiral angle scaleFactor n line
  = spiral' n line
  where
    spiral' n line@(p1, p2)
      | n <= 0    = []
      | otherwise = (red, [p2, p1]) : spiral' (n - 1) newLine
      where
        newLine = connectLine (changeLine line) (scaleLine scaleFactor (rotateLine (changeAngle angle) line))
        
changeAngle :: Float -> Float
changeAngle a = a+(pi/71)

changeLine :: Line -> Line
changeLine ((x1,y1),(x2,y2)) = ((x1,y1),(x2*t,y2*t))
        where s = (2*0.1)
              t = (0.7^3)

changeColour :: Colour -> Colour
changeColour (r, g, b, o) = ((r-2), (g+2), (b+2), o)


connectLine :: Line -> Line -> Line
connectLine (_, pE) line2 = startLineFrom pE line2


startLineFrom :: Point -> Line -> Line
startLineFrom startPoint@(x0, y0) ((xS, yS), (xE, yE))
  = (startPoint, ((x0 + xE - xS, y0 + yE - yS))) 

movePoint :: Point -> Point
movePoint (x,y) = (x-5,y+5)
