module Main where 

import Snake 

import Graphics.Gloss 

import Graphics.Gloss.Interface.Pure.Game 

type World = Cobra 

window :: Display 
window = InWindow "Jogo Cobrinha" (1200,720) (500,500) 

desenha :: World -> Picture 
desenha (Cobra direction points) = Pictures $ map desenhaPonto points  

desenhaPonto :: Points -> Picture 
desenhaPonto (x,y) = Color green $ 
                     Translate (tamanho * x) (tamanho * y) $ quadrado tamanho 
       where 
            tamanho = 10 

quadrado :: Float -> Picture 
quadrado l = Polygon $ rectanglePath l l      

reageEventos :: Event -> World -> World 
reageEventos _ w = w 

reageTempo :: Float -> World -> World 
reageTempo _ w = atualiza w  

backgroud :: Color 
backgroud = greyN 0.6 

frameRate :: Int 
frameRate = 10 

main = 
    play window backgroud frameRate jogoInicio desenha reageEventos reageTempo 
      