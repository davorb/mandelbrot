-- mandelbrot.hs
-- (c) 2011 Davor Babic <davor@davor.se>
import Complex
                        
z :: (Num a2, Num a) => a -> a2 -> a2
z 0 _ = 0
z k c = (z (k-1) c)*(z (k-1) c) + c

isPartOfSet a = test a 1                
  where test a k | k > 19 = True
                 | realPart (z k a) > 2  = False
                 | otherwise             = test a (k+1)

init_x = (-2.1)
init_y = (-1.4)
step_x = 0.025
step_y = 0.07

render x y | x >= 0.8 = '\n' : render init_x (y+step_y)
           | y >= 1.4 = []
           | isPartOfSet (x :+ y)  = '.' : render (x+step_x) y
           | otherwise             = ' ' : render (x+step_x) y

mandel = putStrLn $ render init_x init_y

main = mandel
