-- // FICHA 4 \\ -- 

import Ficha4 
import Test.HUnit 

-- Tests -- 

--   2. 

test21 = [[2,1,3],[3,2,1],[1,2,3]] ~=? lMatrixFtoL matrixEx1  
test22 = [[1,2,3]]                 ~=? lMatrixFtoL matrixEx3 

tests2 = TestList [ 
         "Test 1 [[1,2,3],[3,2,1],[2,1,3]]" ~: 
          [[2,1,3],[3,2,1],[1,2,3]]         ~=? lMatrixFtoL matrixEx1, 
         "Test 2 [[1,2,3]]"                 ~:
          [[1,2,3]]                         ~=? lMatrixFtoL matrixEx3
                  ] 


--   3.

test31 = [[3,2,1],[1,2,3],[3,1,2]] ~=? cMatrixFtoL matrixEx1 
test32 = []                        ~=? cMatrixFtoL matrixEx2  

tests3 = TestList [
         "Test 1 [[1,2,3],[3,2,1],[2,1,3]]" ~:
         [[3,2,1],[1,2,3],[3,1,2]]          ~=? cMatrixFtoL matrixEx1,
         "Test 2 []"                        ~: 
         []                                 ~=? cMatrixFtoL matrixEx2 
                  ]


--   4. 

test41 = 3 ~=? elemMatrix (0,1) matrixEx1 
test42 = 2 ~=? elemMatrix (1,0) matrixEx3 

tests4 = TestList [
         "Test 1 [[1,2,3],[3,2,1],[2,1,3]]" ~: 
          3                                 ~=? elemMatrix (0,1) matrixEx1,
         "Test 2 [[1,2,3]]"                 ~: 
          2                                 ~=? elemMatrix (1,0) matrixEx3
                  ]

--   6.

test61 = [[1,2,3],[3,5,1],[2,1,3]] ~=? elemFelem (1,1) 5 matrixEx1 
test62 = matrixEx1                 ~=? elemFelem (4,4) 5 matrixEx1 
test63 = [[1,5,3]]                 ~=? elemFelem (1,0) 5 matrixEx3 

tests6 = TestList [
         "Test 1 [[1,2,3],[3,2,1],[2,1,3]]" ~: 
           [[1,2,3],[3,5,1],[2,1,3]]        ~=? elemFelem (1,1) 5 matrixEx1,
         "Test 2 [[1,2,3],[3,2,1],[2,1,3]]" ~: 
           [[1,2,3],[3,2,1],[2,1,3]]        ~=? elemFelem (4,4) 5 matrixEx1, 
          "Test 3 [[1,2,3]]"                ~: 
           [[1,5,3]]                        ~=? elemFelem (1,0) 5 matrixEx3
                  ]