-- // FICHA 4 \\ --   
  
module Ficha4 where  

type Matrix a = [[a]] 

--   1. 
-- Exmple of a Matriz 
matrixEx1 :: Matrix Int 
matrixEx1 = [ [1,2,3],
           [3,2,1],
           [2,1,3] ] 

matrixEx2 :: Matrix Int 
matrixEx2 = []

matrixEx3 :: Matrix Int 
matrixEx3 = [[1,2,3]] 
 
--   2. 
-- Trade the first line with the last of a given matrix 
lMatrixFtoL :: Matrix a -> Matrix a  
lMatrixFtoL []
           = error "Invalid matrix"
lMatrixFtoL  [x]
           = [x]  
lMatrixFtoL (h:t) 
           = last t : init t ++ [h]  

--   3. 
-- Trades the last colune with the first of a given matrix 
cMatrixFtoL :: Matrix a -> Matrix a 
cMatrixFtoL  []
           = [] 
cMatrixFtoL ((h1:t1):t) 
           = (last t1 : init t1 ++ [h1]) : cMatrixFtoL t      

--   4. 
-- Gives an asked element of a matrix  
elemMatrix :: (Int,Int) -> Matrix a -> a 
elemMatrix _ []
          = error "Invalid matrix"
elemMatrix (c,l) m  
          = (m !! l) !! c  

{-
--   5.
-- Searches for the positon of a given number in a matrix 
nelemM :: a -> Matrix a -> Maybe (Int,Int) 
nelemM _ [] 
      = Nothing 
nelemM x (h:t) 
      = Just (wchL x 0 (h:t), wchC x 0 (wchl x (h:t)))    
 where 
       wchl :: a -> Matrix a -> [a]  
       wchl x (h:t) 
           | elem x h = h  
           | otherwise 
            = wchl x t
       -- 
       wchL :: a -> Int -> Matrix a -> Int      
       wchL x acc (h:t) 
           | elem x h = acc 
           | otherwise 
            = wchL x (acc + 1) t 
       -- 
       wchC :: a -> Int -> Matrix a -> Int  
       wchC x acc (h:t) 
           | x == h = acc
           | otherwise 
            = wchC (acc + 1) t  
-}                   

--   6. 
-- Substitutes a chossen element for another given one 
elemFelem :: (Int,Int) -> a -> Matrix a -> Matrix a 
elemFelem _ _ []
         = []  
elemFelem (c,l) x (h:t) 
         | c == 0 && l == 0 = (x : tail h): t 
         |           l == 0 =  elemL c x h : t 
         | otherwise 
          = h : elemFelem (c, l - 1) x t 
    where 
          elemL :: Int -> a -> [a] -> [a] 
          elemL c x (h:t)  
               | c == 0 = x : t    
               | otherwise 
                = h : elemL (c - 1) x t 

