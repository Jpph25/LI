-- // FICHA 5 \\ -- 

module Main where 

import Graphics.Gloss 

-- Exemplo 

window :: Display 
window = InWindow "Exemplo Aula 5" (1200,720)  (0,0)

background :: Color 
background = magenta 

image :: Picture 
image = Circle 50  

main = display window background image 
