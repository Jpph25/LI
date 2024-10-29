--   // EXEMPLOS \\

import Test.HUnit 

-- Exemple 1  

mysum :: Integer -> Integer -> Integer
mysum x y = x + y 

 -- Tests 

test1  = TestCase (assertEqual "for 1+2,"     3   (mysum 1   2  ))
-- test1 = 3 ~=? mysum 1 2
test2  = TestCase (assertEqual "for 5+5,"     10  (mysum 5   5  ))
test3  = TestCase (assertEqual "for 100+100," 100 (mysum 100 100))

  -- To test more than one test at a time   
tests  = TestList [
        TestLabel "Test 1 (1, 2)"     test1,  
        TestLabel "Test 2 (5, 5)"     test2,
        TestLabel "Test 3 (100, 100)" test3
                 ]

-- Or 
tests1 = TestList [ 
        "Test 1 (1,2)"     ~: 3   ~=? mysum 1   2,
        "Test 2 (5,5)"     ~: 10  ~=? mysum 5   5,
        "Test 3 (100,100)" ~: 100 ~=? mysum 100 100 
                  ] 

-- Exemple 2 

mydiv :: Float -> Float -> Float
mydiv x y = x / y

mydiv1 :: Int -> Int -> Int
mydiv1 x y = div x y

 -- Tests 
test1' = "for md 1/3"   ~: 0.33 ~=? mydiv  1  3  
test2' = "for md 5/0"   ~: 0    ~=? mydiv  5  0 
test3' = "for md1 5/0"  ~: 0    ~=? mydiv1 5  0
test4' = "for md1 15 3" ~: 5    ~=? mydiv1 15 3 

tests' = TestList [
         "for md 1/3"   ~: 0.33 ~=? mydiv  1  3,
         "for md 5/0"   ~: 0    ~=? mydiv  5  0,
         "for md1 5/0"  ~: 0    ~=? mydiv1 5  0,
         "for md1 15 3" ~: 5    ~=? mydiv1 15 3
                  ] 

{-

- test1' : Failed - because the expected resulte is a recurring decimal 
- test2' : Failed - because the espected result is infinity
- test3' : Error  - because the function div is only for integer division 
- test4' : Suceded 

-}

