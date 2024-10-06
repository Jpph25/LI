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
inFig (Square (xq,yq) l) (x3,y3) 
     | x3 > (xq + l) || x3 < xq  
      = "The point dose not belong in the figure"  
     | y3 > (yq + l) || y3 < yq  
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

smallSquare  (Square (xq,yq) l)  
           = (Square (xq,yq) l)  

--   c)
-- Calculate the bigger circle contained inside the figure
bigCircle :: Figure -> Figure
bigCircle (Square (xq,yq) l) 
         = let xc = xq + l/2
               yc = yq + l/2
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
-- Verifies if the first figure is contained in the second   
containedI :: Figure -> Figure -> Bool 
containedI (Circle (xc1,yc1) r1) (Circle (xc2,yc2) r2)
          = let d = sqrt ((xc1 - xc2)^2 + (yc1 - yc2)^2)
            in (d + r1) <= r2 

containedI (Square (xq1,yq1) l1) (Square (xq2,yq2) l2)             
          = let xtd = xq1 + l1 * sqrt 2 
                ytd = yq1 + l1 * sqrt 2 
                d   = sqrt ((xq2 - xtd)^2 + (yq2 - ytd)^2) 
            in  d  <= l2 * sqrt 2  

comtainedI (Rectangle (xr11,yr11) (xr12,yr12)) (Rectangle (xr21,yr21) (xr22,yr22))
          =    (min xr12 xr11) >= (min xr21 xr22) 
            && (min yr12 yr11) >= (min yr21 yr22)
            && (max xr12 xr11) <= (max xr21 xr22) 
            && (max yr12 yr11) <= (max yr21 yr22) 
