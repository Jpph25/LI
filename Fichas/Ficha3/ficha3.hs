{-|
Module      : Ficha 3 
Description : Haskell module containing recursive functions.
Copyright   : (c) João <a110587@uminho.pt>, 2024
Maintainer  : a110587@uminho.pt 

This module contains Haskell definitions to calculate simple recursive 
function
-}

module Ficha3 where 

{-| 
This function removes all words from a list that starts with a given character 

@
rembls  [] _ 
      = [] 
rembls (h:t) a 
      | head h == a = rembls t a   
      | otherwise 
       = h : rembls t 
@ 

==__Notes:__
prop> If the given list is empty, gives back an empty list 

==__Exemples:__
>>>rembls  ["João","Catarina"] 'C'
["João"] 
>>>rembls [] 'J' 
[]

-}
rembls ::  
    [String] -- ^ List of words  
 -> Char     -- ^ Charcter of reference 
 -> [String] -- ^ New list 
rembls  [] _ 
      = [] 
rembls (h:t) a 
      | head h == a = rembls t a   
      | otherwise 
       = h : rembls t a


{-| 
This function adds a given nunber to all the first elements of the pairs in a list of pairs

@
addFst  [] _ 
      = []
addFst ((x,y):t) z 
      = (x + z, y) : addFst t z 
@

==__Notes:__
prop> If the list is empty gives back an empty list

==__Exemples:__
>>> [(0,1),(1,2)] 1 
[(1,1),(2,2)]
>>> [] 2 
[]

-}
addFst :: 
    [(Int,Int)] -- ^ List of pairs  
 -> Int         -- ^ Number to add in the fisrt element of all pairs of the list
 -> [(Int,Int)] -- ^ New List 
addFst  [] _ 
      = []
addFst ((x,y):t) z 
      = (x + z, y) : addFst t z       


{-|
This function calculates the biggest of all the second elements of a lsit of pairs 

@ 
bgg2  []
    = error "Invalid List"
bgg2 [(x,y)]
    = y 
bgg2 ((x,y):(z,w):t)
    | y >= w = bgg2 ((x,y):t) 
    | otherwise 
     = bgg2 ((z,w):t) 
@

==__Notes:__
prop> If the list is empty gives back an error "Invalid List"
prop> If the list has only one pair gives back the second element 

==__Exemples:__
>>> [(0,11),(1,20),(2,3)]
20
>>> [(0,11)]
11
>>> []
error "Invalid List"

-}
bgg2 :: 
    [(Int,Int)] -- ^ List of pairs 
 -> Int         -- ^ The biggest of all the sconde elements of the pairs of the list
bgg2  []
    = error "Invalid List"
bgg2 [(x,y)]
    = y 
bgg2 ((x,y):(z,w):t)
    | y >= w = bgg2 ((x,y):t) 
    | otherwise 
     = bgg2 ((z,w):t) 


type Name = String

type Coordinate = (Double, Double)

-- | Possible movementes 
data Movement = N -- ^ North 
               | S -- ^ South 
               | E -- ^ East
               | W -- ^ West 
      deriving (Show,Eq) 

type Movements = [Movement] 

-- | Indicates the possicion of someone 
data PositionPersson 
   = Pos 
     Name       -- ^ Name of the person 
     Coordinate -- ^ Coordinates of the person  
   deriving (Show,Eq)


{-|
This function makes the cordinates of evryone in a list move one unite to one direction

@
positonM [] _ 
        = [] 
/If they move to __North__ (Up)/            
positonM ((Pos n (x,y)):t) N 
        = Pos n (x,y + 1) : positonM t N  
/If they move to __South__ (Down)/
positonM ((Pos n (x,y)):t) S 
        = Pos n (x,y - 1) : positonM t S 
/If they move to __East__ (Right)/ 
positonM ((Pos n (x,y)):t) E 
        = Pos n (x + 1,y) : positonM t E 
/If they move to __West__ (Left)/
positonM ((Pos n (x,y)):t) W 
        = Pos n (x - 1,y) : positonM t W  
@

==__Notes:__
prop> If the list is empty gives back an empty list 

==__Exemples:__
>>> [Pos "Joao" (1,1),Pos "Catarina" (2,2)] N
[Pos "Joao" (1.0,2.0),Pos "Catarina" (2.0,3.0)]
>>> [Pos "Joao" (1,1),Pos "Catarina" (2,2)] S
[Pos "Joao" (1.0,0.0),Pos "Catarina" (2.0,1.0)]
>>> [Pos "Joao" (1,1),Pos "Catarina" (2,2)] E
[Pos "Joao" (2.0,1.0),Pos "Catarina" (3.0,2.0)]
>>> [Pos "Joao" (1,1),Pos "Catarina" (2,2)] W
[Pos "Joao" (0.0,1.0),Pos "Catarina" (1.0,2.0)]
>>> [] N 
[]

-}
positonM ::
    [PositionPersson] -- ^ List of people and their coordinates  
 -> Movement          -- ^ Direction that they will move
 -> [PositionPersson] -- ^ New list 
positonM [] _ 
        = [] 
positonM ((Pos n (x,y)):t) N 
        = Pos n (x,y + 1) : positonM t N  
positonM ((Pos n (x,y)):t) S  
        = Pos n (x,y - 1) : positonM t S 
positonM ((Pos n (x,y)):t) E  
        = Pos n (x + 1,y) : positonM t E 
positonM ((Pos n (x,y)):t) W 
        = Pos n (x - 1,y) : positonM t W      

{-| 
This function move a person a certain amount of times in any direction

@
multPstn x [] 
        = x      
multPstn (Pos n (x,y)) (h:t) 
       = multPstn (mov (Pos n (x,y)) h) t    
/Auxiliary function that calculates the movement in the head of the list/ 
  where 
       /If it moves __North__ (Up)/
        mov (Pos n (x,y)) N 
           = Pos n (x,y + 1) 
       /If it moves __South__ (Down)/
        mov (Pos n (x,y)) S 
           = Pos n (x,y - 1) 
       /If it moves __East__ (Right)/
        mov (Pos n (x,y)) E 
           = Pos n (x + 1,y) 
       /If it moves __West__ (Left)/
        mov (Pos n (x,y)) W 
           = Pos n (x - 1,y)  
@

==__Notes:__
prop> If the list of movements is empty gives back the same coordinates

==__Exemples:__
>>> (Pos "Joao" (0,0)) [N,E,E]
Pos "Joao" (2.0,1.0)
>>> (Pos "Joao" (0,0)) [S,S,W]
Pos "Joao" (-1.0,-2.0) 
>>> (Pos "Joao" (0,0)) []
Pos "Joao" (0,0) 
-}
multPstn :: 
    PositionPersson -- ^ The person and their coordinates
 -> Movements       -- ^ List of movementes
 -> PositionPersson -- ^ The person with the new coodinates 
multPstn x [] 
        = x      
multPstn (Pos n (x,y)) (h:t) 
       = multPstn (mov (Pos n (x,y)) h) t    
  where 
        mov (Pos n (x,y)) N 
           = Pos n (x,y + 1) 
        mov (Pos n (x,y)) S 
           = Pos n (x,y - 1) 
        mov (Pos n (x,y)) E 
           = Pos n (x + 1,y) 
        mov (Pos n (x,y)) W 
           = Pos n (x - 1,y)     

{-|
This function moves a list of people a certain amount of times in any direction 

@
multPstnM  [] _ 
         = [] 
multPstnM  l []  
         = l 
multPstnM (Pos n (x,y):t) (h:t1) 
         = multPstn (Pos n (x,y)) (h:t1) : multPstnM t (h:t1)
@

==__Notes:__
prop> If the list of people is empty gives back an empty list 
prop> If the list of movements is empty gives back the same list of people 

==__Exemples:__
>>> [Pos "Joao" (0,0), Pos "Catarina" (0,0)] [S,S,W]
[Pos "Joao" (-1.0,-2.0),Pos "Catarina" (-1.0,-2.0)]
>>> [] [S,S,W] 
[]
>>> [Pos "Joao" (0,0), Pos "Catarina" (0,0)] [] 
[Pos "Joao" (0,0), Pos "Catarina" (0,0)] 

-}

multPstnM :: 
    [PositionPersson] -- ^ List of people and their coordinates 
 -> Movements         -- ^ List of movements
 -> [PositionPersson] -- ^ List of people and their coordinates after moving 
multPstnM  [] _ 
         = [] 
multPstnM  l []  
         = l 
multPstnM (Pos n (x,y):t) (h:t1) 
         = multPstn (Pos n (x,y)) (h:t1) : multPstnM t (h:t1) 

{-|
This function indentifies the person positioned more to the north

@
personN [] 
       = Nothing 
personN [x] 
       = Just x 
personN (Pos n1 (x1,y1): Pos n2 (x2,y2): t) 
       = Just (north (Pos n1 (x1,y1):Pos n2 (x2,y2):t))    
  where 
/Auxiliar function that /indentifies the person position more to the __north__ (Bigger y coordinate)/  
        north [x] 
             = x  
        north (Pos n1 (x1,y1):Pos n2 (x2,y2):t)
             | y1 >= y2 = north (Pos n1 (x1,y1):t) 
             | otherwise 
              = north (Pos n2 (x2,y2):t)
@

==__Notes:__
prop> If the list is empty gives back nothing 
prop> If the list has only one element, gives back that same element 

==__Exemples:__
>>> [Pos "Joao" (0,1), Pos "Catarina" (0,2)]
Just (Pos "Catarina" (0.0,2.0))
>>> [Pos "Joao" (0,1)]
Just (Pos "Joao" (0.0,1.0))
>>> personN []
Nothing 

-}
personN ::  
    [PositionPersson]     -- ^ List of people and their coordinates  
 -> Maybe PositionPersson -- ^ The person more to the __north__ /(Bigger y coordinate)/ 
personN [] 
       = Nothing 
personN [x] 
       = Just x 
personN (Pos n1 (x1,y1): Pos n2 (x2,y2): t) 
       = Just (north (Pos n1 (x1,y1):Pos n2 (x2,y2):t))    
  where 
        north [x] 
             = x  
        north (Pos n1 (x1,y1):Pos n2 (x2,y2):t)
             | y1 >= y2 = north (Pos n1 (x1,y1):t) 
             | otherwise 
              = north (Pos n2 (x2,y2):t)  

{-|
This functions gives the list of names of everyone that is more to the north 

@
peopleN  []
       = []  
peopleN (Pos n (x,y):t) 
       = listN (Pos n (x,y):t) maxY 
  where 
/Auxiliar function that tranformas the type of the result the function maxN into a Double/
        maxY = maxN (Pos n (x,y):t) 
        --
/Auxiliar function that calculates the coordinates of the person more to the __north__ (Bigger y coordinate)/        
        maxN :: 
            [PositionPersson] (List of people and their coordinates) 
         -> Double            (Coordinates of the people more to the south)
        maxN [Pos n (x,y)]
            = y
        maxN (Pos n1 (x1,y1):Pos n2 (x2,y2):t)         
            | y1 >= y2 = maxN (Pos n1 (x1,y1):t) 
            | otherwise 
             = maxN (Pos n2 (x2,y2):t) 
        --
/Auxiliar function that makes the list of names of everyone in a certain coordinate/  
        listN :: 
            [PositionPersson] (List of people and their coordinates) 
         -> Double            (Coordinates of the people more to the south)
         -> [Name]            (List of names of everyone more to the north) 
        listN  [] c 
             = []   
        listN (Pos n (x,y):t) c 
             | y == c = n : listN t c 
             | otherwise 
              = listN t c
@

==__Notes:__
prop> If the list of people is empty than it gives back an empty list
prop> If thres only one element than it gives back an list with that persons name 

==__Exemples:__
>>> [Pos "Joao" (1.0, 5.0), Pos "Catarina" (2.0, 10.0),Pos "Carvalhosa" (4.0, 10.0)]
["Catarina","Carvalhosa"]
>>> [Pos "Joao" (1.0, 5.0)]
["Joao"]
>>> []
[] 

-}
peopleN :: 
    [PositionPersson] -- ^ List of people and their coordinates 
 -> [Name]            -- ^ List of names of everyone more to the north 
peopleN  []
       = []  
peopleN (Pos n (x,y):t) 
       = listN (Pos n (x,y):t) maxY 
  where 
        maxY = maxN (Pos n (x,y):t) 
        --
        maxN :: [PositionPersson] -> Double
        maxN [Pos n (x,y)]
            = y
        maxN (Pos n1 (x1,y1):Pos n2 (x2,y2):t)         
            | y1 >= y2 = maxN (Pos n1 (x1,y1):t) 
            | otherwise 
             = maxN (Pos n2 (x2,y2):t) 
        --
        listN :: [PositionPersson] -> Double -> [Name] 
        listN  [] c 
             = []   
        listN (Pos n (x,y):t) c 
             | y == c = n : listN t c 
             | otherwise 
              = listN t c  

{-|
This function moves all elementes of a list a certain number of times to the right 

@
ntoRight  l 0 
        = l 
ntoRight  [] _
        = [] 
ntoRight (h:t) n 
        = ntoRight (last t : h : init t) (n - 1)
@

==__Notes:__
prop> If the list is empty gives back an empty list 
prop> If the number gib«ven is 0 gives back the same list 
prop> The elements in the end of the list move to the begning 

==__Exemples:__
>>> [1,2,3,4,5] 2
[4,5,1,2,3]
>>> [1,2,3,4,5] 0
[1,2,3,4,5]
>>> [] 1
[]

-}
ntoRight ::
    [a] -- ^ List 
 -> Int -- ^ How many times the elements move to the right 
 -> [a] -- ^ List after the elements moved 
ntoRight  l 0 
        = l 
ntoRight  [] _
        = [] 
ntoRight (h:t) n 
        = ntoRight (last t : h : init t) (n - 1)  