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
nPrimUltv l = if   length l == 0 
              then " "
              else head l ++ " " ++ last l 

--  d)
tApel :: [String] -> Int
tApel l = if   length l == 0
          then 0
          else length (last l)

--  e) 
pertChar :: Char -> String -> Bool  
pertChar a l = elem a l         

--  f) 
apag :: [a] -> [a] 
apag l = if   even (length l) 
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

-- iv. 
pertnTurno :: Eq a => [a] -> a -> String
pertnTurno l a = if   elem a l == True 
                 then "Sim"
                 else "Nao"

--  h)
somaOrdl :: [Int] -> [Int] -> [Int]
somaOrdl a b = if   length a > length b 
               then b ++ a 
               else a ++ b   

--  i) 
somaOrdh :: [Int] -> [Int] -> [Int]
somaOrdh a b = if   head a > head b 
               then b ++ a
               else a ++ b

--  j)
parList :: ([a],[a]) -> (a,[a])
parList (a,b) = (head a,b) 


--  k)
nomePlUltn :: [String] -> String 
nomePlUltn a = [head(head a)] ++ "." ++ last a 

--  l) 
parListInt :: [(Int,Int)] -> Int
parListInt a = if   length a == 0
               then 0
               else snd(head a)   

--  m)
sumParList :: [(Int,Int)] -> Int
sumParList a = if   length a == 0
               then 0
               else fst(head a) + snd (head a)

--  n) 
personYoung :: (String,Int) -> (String,Int) -> String
personYoung (a,b) (c,d) = if   b > d
                          then c
                          else a 

--  o)
f :: (Int,Int) -> Int -> (Int,Int)
f (x,y) l = (x + l, y - l)

-- p) 
g :: ((Int, Int), Int) -> ((Int, Int), Int) -> Int 
g ((x1,y1), l1) ((x2,y2), l2) = if   y1 == y2 
                                then if   l1 > l2
                                     then l2^2
                                     else l1^2
                                else if   y1 > y2 
                                     then l1^2
                                     else l2^2
                                        
