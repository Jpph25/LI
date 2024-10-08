--   // FICHA 2 \\

-- 1. 

data Movement =  North 
               | South 
               | East 
               | West  
               deriving Show

type Point = (Double,Double) 

--   a)
-- Calculates the coordinates of the point after a movement
move :: Point -> Movement -> Point 
move (x,y) North 
    = (x, y + 1) 
move (x,y) South 
    = (x, y - 1)
move (x,y) East  
    = (x + 1, y)
move (x,y) West  
    = (x - 1, y)   

--   b) 
-- Calculates the distance between two points
dist2p :: Point -> Point -> Double
dist2p (x1,y1) (x2,y2) 
      = sqrt ((x1 - x2)^2 + (y1 - y2)^2) 

--   c)
-- Calculates wich point is more to the south 
-- If its the same gives the first point    
pointS :: Point -> Point -> Point 
pointS (x1,y1) (x2,y2) 
      | y1 == y2 = (x1,y1)
      | y1 >  y2 = (x1,y1)
      | y1 <  y2 = (x2,y2)  


-- 2. 
-- Calculates the coordinates of the point after a movement
-- Making sure it dosent leave the window
move' :: Point -> Movement -> Double -> Point 
move' (x,y) North d 
     = (x, min (y + 1) d) 
move' (x,y) South d 
     = (x, max (y - 1) 0) 
move' (x,y) East  d 
     = (min (x + 1) d, y)
move' (x,y) West  d 
     = (max (x - 1) 0, y)   


-- 3. 
-- Calculates the coordinates of a point in case the origin was the topleft conner 
infAsup :: Point -> Double -> Point  
infAsup (x,y) l 
       = (x, l + y)  


-- 4.
-- Calculates the coordinates of a point in case the origin was the center 
infAcent :: Point -> Double -> Point  
infAcent (x,y) l 
        = (x + l/2, y + l/2)


-- 5.

type Speed = Double

type Time  = Double

-- Calculates the coordinates of a point after moving horizontaly 
-- in a constante speed in a given time
moveSpedx :: Point -> Speed -> Time -> Point
moveSpedx (x,y) v t 
         = (x + v * t, y)   


-- 6.  
-- Calculates the coordinates of a point after moving verticaly 
-- in a constante speed in a given time
moveSpedy :: Point -> Speed -> Time -> Point
moveSpedy (x,y) v t 
         = (x , y + v * t) 


-- 7. 
-- Calculates the coordinates of a point after moving in a constante speed in a given time
type Speed' = (Double, Double)

moveSpedxy :: Point -> Speed' -> Time -> Point
moveSpedxy (x,y) (vx,vy) t 
          = (x + vx * t, y + vy * t) 

-- 8

data Figure = Circle    Point Double   |
              Rectangle Point Point    | 
              Square    Point Double 
             deriving (Show,Eq)

--   a) 
-- Verifies if a given point is inside a figure
inFig :: Figure -> Point -> String 
inFig (Circle (xc,yc) r) (x1,y1) 
     = if   sqrt ((xc - x1)^2 + (yc - y1)^2) > r
       then "The point dose not belong in the figure"
       else "The point belonges in the figure"

inFig (Rectangle (xr1,yr1) (xr2,yr2)) (x2,y2) 
     | x2 > min xr1 xr2 || x2 < max xr1 xr2 
      = "The point dose not belong in the figure"  
     | y2 > min yr1 yr2 || y2 < max yr1 yr2 
      = "The point dose not belong in the figure"    
     | otherwise = "The point belonges in the figure" 
inFig (Square (xs,ys) l) (x3,y3) 

     | x3 > (xs + l) || x3 < xs  
      = "The point dose not belong in the figure"  
     | y3 > (ys + l) || y3 < ys  
      = "The point dose not belong in the figure"    
     | otherwise = "The point belonges in the figure"  

--   b)
-- Calculates the samller square that contains a given figure 
smallSquare :: Figure -> Figure 
smallSquare  (Circle (xc,yc) r) 
           = (Square (xc- r, yc - r) (2 * r))

smallSquare  (Rectangle (xr1,yr1) (xr2,yr2)) 
           = (Square (min xr1 xr2, max yr1 yr2) 
              (max (abs (xr1 - xr2)) (abs (yr1-yr2)))) 

smallSquare  (Square (xs,ys) l)  
           = (Square (xs,ys) l)  

--   c)
-- Calculate the bigger circle contained inside the figure
bigCircle :: Figure -> Figure
bigCircle (Square (xs,ys) l) 
         = let xc = xs + l/2
               yc = ys + l/2
               r  = l/2   
           in (Circle (xc,yc) r) 

bigCircle (Rectangle (xr1,yr1) (xr2,yr2))
         = let xc = (xr1 + xr2)/2 
               yc = (yr1 + yr2)/2 
               r  = min ((abs (xr1 - xr2))/2) ((abs (yr1 -yr2))/2) 
           in (Circle (xc,yc) r) 

bigCircle  (Circle (xc,yc) r)
         = (Circle (xc,yc) r) 

--   d)
-- Verifies if the first figure is contained in the second (Same figure)  
containedI :: Figure -> Figure -> Bool 
containedI (Circle (xc1,yc1) r1) (Circle (xc2,yc2) r2)
          = let d = sqrt ((xc1 - xc2)^2 + (yc1 - yc2)^2)
            in (d + r1) <= r2 

containedI (Square (xs1,ys1) l1) (Square (xs2,ys2) l2)             
          = let xtd = xs1 + l1 * sqrt 2 
                ytd = ys1 + l1 * sqrt 2 
                d   = sqrt ((xs2 - xtd)^2 + (ys2 - ytd)^2) 
            in  d  <= l2 * sqrt 2  

containedI (Rectangle (xr11,yr11) (xr12,yr12)) (Rectangle (xr21,yr21) (xr22,yr22))
          =    (min xr12 xr11) >= (min xr21 xr22) 
            && (min yr12 yr11) >= (min yr21 yr22)
            && (max xr12 xr11) <= (max xr21 xr22) 
            && (max yr12 yr11) <= (max yr21 yr22) 

--   e) 
-- Verifies if the first figure is contained in the second (Different figures)
containedD :: Figure -> Figure -> Bool 
containedD (Circle (xc,yc) r) (Rectangle (xr1,yr1) (xr2,yr2)) 
          = let dx1 = abs (xc - xr1) 
                dx2 = abs (xc - xr2)
                dy1 = abs (yc - yr1)
                dy2 = abs (yc - yr2) 
            in     (min dx1 dx2) >= r  && (min dy1 dy2) >= r 
                && (min xr1 xr2) <  xc && (min yr1 yr2) <  yc  
                && (max xr1 xr2) >  xc && (max yr1 yr2) >  yc 

containedD  (Circle (xc,yc) r) (Square (xs,ys) l)
          = let dx = abs (xc - xs) 
                dy = abs (yc - ys) 
            in      dx      >= r  &&  dy      >= r 
                &&  xs      <  xc &&  ys      <  yc 
                && (xs + l) >  xc && (ys + l) >  yc

containedD (Square (xs,ys) l) (Circle (xc,yc) r) 
          = let d1  = sqrt ((xs - xc)^2 + (ys - yc)^2) 
                xs2 = xs + l
                ys2 = ys + l
                d2  = sqrt ((xs2 - xc)^2 + (ys2 - yc)^2) 
            in (max d1 d2) < r 

containedD (Square (xs,ys) l) (Rectangle (xr1,yr1) (xr2,yr2)) 
          = let xs2 = xs + l
                ys2 = ys + l 
            in     (min xr1 xr2) <= xs && (min yr1 yr2) <= ys
                && (max xr1 xr2) >= xs && (max yr1 yr2) >= ys 

containedD (Rectangle (xr1,yr1) (xr2,yr2)) (Circle (xc,yc) r)
          = let dm = sqrt (((min xr1 xr2) - xc)^2 + ((min yr1 yr2) - yc)^2)
                dM = sqrt (((max xr1 xr2) - xc)^2 + ((max yr1 yr2) - yc)^2)
            in (max dm dM) <= r 

containedD (Rectangle (xr1,yr1) (xr2,yr2)) (Square (xs,ys) l) 
          = let xs2 = xs + l
                ys2 = ys + l
            in     (min xr1 xr2) >= xs && (min yr1 yr2) >= ys
                && (max xr1 xr2) <= xs && (max yr1 yr2) <= ys     

--   f)
-- Makes a figure bigger or smaller
zoom :: Figure -> Double -> Figure
zoom  (Circle (xc,yc) r) f
    = (Circle (xc,yc) (r * f)) 

zoom  (Square (xs,ys) l) f 
    = (Square (xs,ys) (l * f)) 

zoom  (Rectangle (xr1,yr1) (xr2,yr2)) f  
    = let lx = abs (xr1 - xr2)
          ly = abs (yr1 - yr2)
          xm = (min xr1 xr2)
          ym = (min yr1 yr2)    
      in (Rectangle (xm,ym) ((xm + lx * f),(ym + ly * f)))   