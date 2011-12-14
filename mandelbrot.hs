-- mandelbrot.hs - (c) 2011 Davor Babic <davor@davor.se>
import Complex

z :: (Num a2, Num a) => a -> a2 -> a2
z 0 _ = 0
z k c = (z (k-1) c)*(z (k-1) c) + c

isPartOfSet a = test a 1
  where test a k | k > iterations = True
                 | realPart (z k a) > 2  = False
                 | otherwise             = test a (k+1)

render x y | x >= max_x = '\n' : render init_x (y+step_y)
           | y >= max_y = []
           | isPartOfSet (x :+ y)  = '*' : render (x+step_x) y
           | otherwise             = ' ' : render (x+step_x) y

main = putStrLn $ render init_x init_y

init_x = (-2.1)
init_y = (-1.4)
max_x  = 0.8
max_y  = 1.4
step_x = 0.04
step_y = 0.07
iterations = 30
