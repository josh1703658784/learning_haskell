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
      | otherwise = (colour, [p1, p2]) : spiralRays' (n-1) newColour newLine
      where
        newColour = fade colour
        newLine   = scaleLine scaleFactor (rotateLine angle line)