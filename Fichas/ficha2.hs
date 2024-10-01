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
             Circulo Ponto Double   |
             Rectangulo Ponto Ponto | 
             Quadrado Ponto Double 
             deriving (Show,Eq)

--  a) 
inFig :: Figura -> Ponto -> String 
inFig (Circulo (xc,yc) r) (x1,y1) = if   sqrt ((xc - x1)^2 + (yc - y1)^2) > r
                                    then "O ponto não pertence a figura"
                                    else "O ponto pertence a figura" 
inFig (Rectangulo (xr1,yr1) (xr2,yr2)) (x2,y2)
                                              | x2 > min xr1 xr2 || x2 < max xr1 xr2 
                                               = "O ponto não pertence a figura"  
                                              | y2 > min yr1 yr2 || y2 < max yr1 yr2 
                                               = "O ponto não pertence a figura"    
                                              | otherwise = "O ponto pertence a figura"
inFig (Quadrado (xq,yq) l) (x3,y3) 
                                  | x3 > (xq + l) || x3 < xq  
                                   = "O ponto não pertence a figura"  
                                  | y3 > (yq + l) || y3 < yq  
                                   = "O ponto não pertence a figura"    
                                  | otherwise = "O ponto pertence a figura" 

--  b) 
menorQuadrado :: Figura -> Figura 
menorQuadrado (Circulo (xc,yc) r) 
               = (Quadrado (xc- r, yc - r) (2 * r)) 
menorQuadrado (Rectangulo (xr1,yr1) (xr2,yr2)) 
               = (Quadrado (min xr1 xr2, max yr1 yr2) 
                 (max (abs (xr1 - xr2)) (abs (yr1-yr2)))) 
menorQuadrado (Quadrado (xq,yq) l)  
               = (Quadrado (xq,yq) l)  

--  c)
maiorCirculo :: Figura -> Figura
maiorCirculo (Quadrado (xq,yq) l) 
              = (Circulo (xq + l/2, yq + l/2) 
                (sqrt ((xq - xq + l/2 )^2 + (yq - yq + l/2)^2)))  
maiorCirculo (Rectangulo (xr1,yr1) (xr2,yr2)) 
              = (Circulo ((xr1 + xr2)/2, (yr1+yr2)/2)
                (sqrt ((min xr1 xr2) - (xr1 + xr2)/2) +  ((max yr1 yr2) - (yr1 + yr2)/2)))
maiorCirculo (Circulo (xc,yc) r)  
              = (Circulo (xc,yc) r) 

-- d)   
contida :: Figura -> Figura -> Bool
contida (Circulo (xc1,yc1) r1) (Circulo (xc2,yc2) r2) 
         = if   (sqrt ((xc1 - xc2)^2 + (yc1 - yc2)^2) + r1) <= r2
           then True
           else False  
contida (Quadrado (xq1,yq1) l1) (Quadrado (xq2,yq2) l2)
         = if   (sqrt ((xq1 - xq2)^2 + (yq1 - yq2)^2) + l1 * sqrt 2) <= l2 * sqrt 2 
           then True
           else False  
contida (Rectangulo (xr11,yr11) (xr12,yr12)) (Rectangulo (xr21,yr21) (xr22,yr22))
         = if (    (min xr12 xr11) >= (min xr21 xr22) 
                 && (min yr12 yr11) >= (min yr21 yr22)
                && (max xr12 xr11) <= (max xr21 xr22) 
                 && (max yr12 yr11) <= (max yr21 yr22))   
           then True
           else False                           