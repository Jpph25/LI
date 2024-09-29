-- Ficha 2 

-- 1 

data Movimento = Norte 
               | Sul 
               | Este 
               | Oeste 
               deriving Show

type Ponto = (Double,Double) 

--  a)
move :: Ponto -> Movimento -> Ponto 
move (x,y) Norte = (x, y + 1) 
move (x,y) Sul   = (x, y - 1)
move (x,y) Este  = (x + 1, y)
move (x,y) Oeste = (x - 1, y)   

--  b) 
dist2p :: Ponto -> Ponto -> Double
dist2p (x1,y1) (x2,y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2) 

--  c) 
pointS :: Ponto -> Ponto -> Ponto 
pointS (x1,y1) (x2,y2) = if   y1 == y2 
                         then (x2,y2) 
                         else if   y1 > y2 
                              then (x1,y1)
                              else (x2,y2)    

-- 2

move' :: Ponto -> Movimento -> Double -> Ponto 
move' (x,y) Norte  d = (x, min (y + 1) d) 
move' (x,y) Sul    d = (x, max (y - 1) 0) 
move' (x,y) Este   d = (min (x + 1) d, y)
move' (x,y) Oeste  d = (max (x - 1) 0, y)   

-- 3 

infAsup :: Ponto -> Double -> Ponto 
infAsup (x,y) l = (x, l + y)  

-- 4

infAcent :: Ponto -> Double -> Ponto 
infAcent (x,y) l = (x + l/2, y + l/2)

-- 5

type Velocidade = Double

type Tempo = Double

moveVelox :: Ponto -> Velocidade -> Tempo -> Ponto
moveVelox (x,y) v t = (x + v * t, y)   

-- 6 

moveVeloy :: Ponto -> Velocidade -> Tempo -> Ponto
moveVeloy (x,y) v t = (x , y + v * t) 

-- 7 

type Velocidade' = (Double, Double)

moveVeloxy :: Ponto -> Velocidade' -> Tempo -> Ponto
moveVeloxy (x,y) (vx,vy) t = (x + vx * t, y + vy * t) 

-- 8

data Figura =
             |Circulo Ponto Double 
             |Rectangulo Ponto Ponto 
             |Quadrado Ponto Double 
             deriving (Show,Eq)

--  a) 
