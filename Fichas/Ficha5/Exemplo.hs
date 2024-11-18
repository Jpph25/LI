-- // EXEMPLO \\ -- 

import Graphics.Gloss  

-- Exemplo 

circle1 :: Picture 
circle1 = Circle 50 

circle2 :: Picture 
-- Moves circle1 -60 in the x axis and 30 in yhe y axis  
circle2 = Translate (-60) 30 
          circle1  

-- Changes the collor of a determined circle 
circler :: Picture 
circler = Color red circle1 

circleb :: Picture 
circleb = Color blue circle2 


-- 1. 

circle3 :: Picture 
circle3 = rotate (-45) 
        $ scale 0.5 1 
        $ Translate (-60) 30 
          circle1  
{-
Makes the circle half the size in the x axis 
Rotates it 45 degres to the left 
and moves it 60 units to the left and 30 up 
-}

-- 2. 

circle4 :: Picture 
circle4 = scale 1 0.5 
        $ color yellow 
        $ circleSolid 20  

-- 3. 

greenSquare :: Picture 
greenSquare = color green 
            $ rectangleSolid 20 20 

-- 4. 
poligonalLine :: Picture
poligonalLine = Line [(0,0), (-200,0), (200,200), (0,200), (0,0)]

-- Makes a list of the tow circles 
circles = Pictures [circler, circleb, circle3, circle4]  

-- Makes a list of the created figures 
figures = Pictures [circles, greenSquare, poligonalLine] 

-- Window 
window :: Display 
window = InWindow 
        "Ficha5 Window"  -- Name of the window 
        (500,500)        -- Size of the window 
        (10,10)          -- Position of the window 

background :: Color 
-- Color of the background
background = greyN 0.8   -- Personalized tone of grey 

main :: IO () 
main = display window background figures  

-- 'runhaskell folder' -> Comand to run the program 