-- Ficha 1

-- 2
--  a)
lPrimUlt :: [a] -> (a,a)
lPrimUlt l = (head l,last l)  

--  b)
nPrimUlt :: [String] -> (String,String)
nPrimUlt l = (head l,last l)

--  c)
nPrimUltv :: [String] -> String
nPrimUltv l = if length l == 0 
              then " "
              else head l ++ " " ++ last l 

--  d)
tApel :: [String] -> Int
tApel l = if length l == 0
          then 0
          else length (last l)

--  e) 
pertChar :: Char -> String -> Bool  
pertChar a l = elem a l         

--  f) 
apag :: [a] -> [a] 
apag l = if even (length l) 
         then tail l
         else init l 

--  g)
-- i.
total :: [a] -> [a] -> [a]
total l a = l ++ a

-- ii.
nTotal :: [a] -> [a] -> Int
nTotal l a = length (l ++ a)

-- iii.
turnosDif :: [a] -> [a] -> Int 
turnosDif a l = abs (length a - length l)

--iv. 
pertnTurno :: Eq a => [a] -> a -> String
pertnTurno l a = if elem a l == True 
                 then "Sim"
                 else "Nao"

