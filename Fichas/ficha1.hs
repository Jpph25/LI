--   // FICHA 1 \\

-- 2.

--   a)
-- Gives a pair with  the first and last element of a given list
lFstLst :: [a] -> (a,a)
lFstLst l 
       = (head l,last l)  

--   b)
-- Gives a pair with the fist anda last name 
nFstLst :: [String] -> (String,String)
nFstLst l 
        = (head l,last l)

--   c)
-- Gives back the fist and last name or nothing in case the list is empty  
nFstLstv :: [String] -> String
nFstLstv l 
        = if   length l == 0 
          then " "
          else head l ++ " " ++ last l 

--   d)
-- Calculates the size of the last name  
sLast :: [String] -> Int
sLast l 
     = if   length l == 0
       then 0
       else length (last l)

--   e)
-- Verifies if a given charcter belongs to a given word 
isinChar :: Char -> String -> Bool  
isinChar a l 
        = elem a l         

--   f) 
-- Removes the first elemnet if length is even, and removes the last
-- if its odd
remv :: [a] -> [a] 
remv l = if   even (length l) 
         then tail l
         else init l 

--   g)
--  i.
-- Calculates the list of the two shifts together
total :: [a] -> [a] -> [a]
total l a 
     = l ++ a

--  ii.
-- Calculates the size of the two shifts together 
nTotal :: [a] -> [a] -> Int
nTotal l a 
      = length (l ++ a)

--  iii.
-- Calculates the difference in size of the two shifts
shiftsDif :: [a] -> [a] -> Int 
shiftsDif a l 
         = abs (length a - length l)

--  iv.
-- Verifies if a student belongs to a given shift  
isinShifts :: Eq a => [a] -> a -> String
isinShifts l a 
          = if   elem a l == True 
            then "Yes"
            else "No"

--   h)
-- Sums two lists making the smaller one first 
sumOrdl :: [Int] -> [Int] -> [Int]
sumOrdl a b 
       = if   length a > length b 
         then b ++ a 
         else a ++ b   

--   i)
-- Sums two lists making the one with the smaller head first 
sumOrdh :: [Int] -> [Int] -> [Int]
sumOrdh a b 
       = if   head a > head b 
         then b ++ a
         else a ++ b

--   j)
-- Gives a pair with the head of the first list and the same last list
pairList :: ([a],[a]) -> (a,[a])
pairList  (a,b) 
       = (head a,b) 


--   k)
-- Gives the first letter of the first name and the last name 
nameFstlst :: [String] -> String 
nameFstlst a 
          = [head(head a)] ++ "." ++ last a 

--   l) 
-- Gives the last element of the head of the list and zero if its empty
parListInt :: [(Int,Int)] -> Int
parListInt a 
          = if   length a == 0
            then 0
            else snd(head a)   

--   m)
-- Sums the fist and last element of the pair of the first element of the list
-- When the list is empty gives zero 
sumPairList :: [(Int,Int)] -> Int
sumPairList a
          = if   length a == 0
            then 0
            else fst(head a) + snd (head a)

--   n)
-- Gives the name of the younger person
personYoung :: (String,Int) -> (String,Int) -> String
personYoung (a,b) (c,d) 
           = if   b > d
             then c
             else a 

--   o)
-- Calculates the coordinates of the lower right vertex
f :: (Int,Int) -> Int -> (Int,Int)
f (x,y) l 
 = (x + l, y - l)

--   p)
-- Calculates the area of the topmost square 
-- or the smaller area in case they are at tha same level  
g :: ((Int, Int), Int) -> ((Int, Int), Int) -> Int 
g ((x1,y1), l1) ((x2,y2), l2) 
 | y1 == y2 && l1 > l2 = l2^2
 | y1 == y2 && l1 < l2 = l1^2
 | y1 >  y2            = l1^2     
 | y1 <  y2            = l2^2

