module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Snake

type World = JogoCobra

desenha :: World -> Picture
desenha j@JogoCobra {cobra = Cobra direcao pontos, maca = maca} =
  Pictures $ desenhaMaca maca : map desenhaPonto pontos

desenhaMaca :: Maca -> Picture
desenhaMaca (x, y) = Color red $ Translate (raio * 2 * x) (raio * 2 * y) $ circleSolid raio
  where
    raio = 10

desenhaPonto :: Ponto -> Picture
desenhaPonto (x, y) = Color green $ Translate (tamanho * x) (tamanho * y) $ quadrado tamanho
  where
    tamanho = 20

quadrado :: Float -> Picture
quadrado l = Polygon $ rectanglePath l l

reageEventos :: Event -> World -> World
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) jogo = movimenta Subir jogo
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) jogo = movimenta Descer jogo
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) jogo = movimenta VirarEsquerda jogo
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) jogo = movimenta VirarDireita jogo
reageEventos _ w = w

reageTempo :: Float -> World -> World
reageTempo _ w = atualiza w

window :: Display
window = InWindow "Jogo da Cobrinha" (1280, 720) (500, 500)

background :: Color
background = white

frameRate :: Int
frameRate = 10

main :: IO ()
main =
  play window background frameRate jogoInicio desenha reageEventos reageTempo
  where
    jogoInicio = JogoCobra {cobra = Cobra Cima [(0, -10), (0, -11), (0, -12), (0, -13)], maca = (15, 15)}
