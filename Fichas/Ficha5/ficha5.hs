-- // FICHA 5 \\ -- 

import Graphics.Gloss 

import Graphics.Gloss.Interface.Pure.Game 

-- NEW TYPES --                             

type Position = (Float, Float) 

type Velocity = (Float, Float) 

type Time = Float 

type GameState = (Position, Velocity, Time)  

-- GAME BEGING --

inicialPostion :: GameState   
inicialPostion = ((0,0),(0,0),0) 

-- DRAWING FUNCTIONS -- 

drawposition :: GameState -> Picture 
drawposition ((x,y), (vx, vy) , time) 
            = Pictures [drawCharacter (x,y) time, drawTime time, drawVelocity (vx, vy)]

-- Velocity meter 
drawVelocity :: Velocity -> Picture 
drawVelocity (vx,vy) = Translate (-190) 160
                     $ Scale 0.1 0.1
                     $ Color (dark blue) 
                     $ Text $    "Velocity: (" ++ show vx ++ ", " ++ show vy ++ ")"    

-- Timer 
drawTime :: Float -> Picture 
drawTime time = Translate (-190) 180 
              $ Scale 0.1 0.1 
              $ Color (dark blue) 
              $ Text $ "Time: " ++ show (round time :: Int) ++ "s"   

-- Character 
drawCharacter :: Position -> Time -> Picture
drawCharacter (x,y) time = Translate x y poligon 
                where 
                      poligon :: Picture 
                      poligon = Color (characterColor time)    
                              $ Polygon [(0,0), (10,0), (10,10), (0,10), (0,0)]
-- Makes the collor of the caracter chage over time 
characterColor :: Time -> Color 
characterColor time 
              | odd (round time :: Int) = dark red 
              | otherwise 
               = dark yellow

-- EVENTES REACTIONS --

reactEvent :: Event -> GameState -> GameState 
reactEvent (EventKey (SpecialKey KeyUp   ) Down _ _ ) ((x,y), (vx,vy), t) 
          = ((x, y), (vx, 50), t)
reactEvent (EventKey (SpecialKey KeyUp   )  Up _ _ ) ((x,y), (vx,vy), t)
          = ((x,y), (vx, 0), t) 

reactEvent (EventKey (SpecialKey KeyDown ) Down _ _ ) ((x,y), (vx,vy), t) 
          = ((x, y), (vx, -50), t)
reactEvent (EventKey (SpecialKey KeyDown ) Up   _ _ ) ((x,y), (vx,vy), t)
          = ((x,y), (vx, 0), t) 

reactEvent (EventKey (SpecialKey KeyRight) Down _ _ ) ((x,y), (vx,vy), t) 
          = ((x, y), (50, vy), t)
reactEvent (EventKey (SpecialKey KeyRight) Up   _ _ ) ((x,y), (vx,vy), t)
          = ((x,y), (0, vy), t) 

reactEvent (EventKey (SpecialKey KeyLeft ) Down _ _ ) ((x,y), (vx,vy), t) 
          = ((x, y), (-50, vy), t)  
reactEvent (EventKey (SpecialKey KeyLeft ) Up   _ _ ) ((x,y), (vx,vy), t)
          = ((x,y), (0, vy), t) 

reactEvent _ s = s   -- Ignores other eventes  

-- OVERTIME EVENTES --

reactTime :: Float -> GameState -> GameState  
reactTime n ((x,y), (vx,vy), time) 
         | (x + vx * n) < (-200) = (( 200, y), (vx, vy), time + n) 
         | (x + vx * n) >   200  = ((-200, y), (vx, vy), time + n) 
         | (y + vy * n) < (-200) = ((x,  200), (vx, vy), time + n) 
         | (y + vy * n) >   200  = ((x, -200), (vx, vy), time + n)  
         | otherwise 
          = ((x + vx * n, y + vy * n), (vx, vy), time + n)

-- GAME -- 

frames :: Int 
frames = 60 

window :: Display 
window = InWindow "New Game" (400, 400) (0, 0) 

main :: IO () 
main = do play window         -- Window of the game 
              (greyN 0.5)     -- Colour of the background
               frames         -- Framerate
               inicialPostion -- Inicial position 
               drawposition   -- Draws the initial position 
               reactEvent     -- Reacts to the eventes   
               reactTime      -- Reacts to the passing of time 