module Snake where

type Ponto = (Float, Float)

type Maca = Ponto

data Direcao = Cima | Baixo | Esquerda | Direita deriving (Show, Eq, Ord)

data Cobra = Cobra {direcao :: Direcao, corpo :: [Ponto]} deriving (Show, Eq)

data JogoCobra = JogoCobra {cobra :: Cobra, maca :: Maca} deriving (Show, Eq)

data Movimento = Subir | Descer | VirarEsquerda | VirarDireita deriving (Show, Eq, Ord)

movimenta :: Movimento -> JogoCobra -> JogoCobra
movimenta m j@JogoCobra {cobra = c@Cobra {direcao = direcao}} = j {cobra = c {direcao = novaDirecao}}
  where
    novaDirecao = case m of
      Subir -> Cima
      Descer -> Baixo
      VirarEsquerda -> Esquerda
      VirarDireita -> Direita

atualiza :: JogoCobra -> JogoCobra
atualiza j@JogoCobra {cobra = c@Cobra {direcao = direcao, corpo = (h : t)}, maca = maca} = j {cobra = c {corpo = cabeca : h : cauda}}
  where
    cabeca = novoPonto direcao h
    cauda = if h == maca then t else init t

novoPonto :: Direcao -> Ponto -> Ponto
novoPonto direcao (x, y) = case direcao of
  Cima -> (x, y + 1)
  Baixo -> (x, y - 1)
  Esquerda -> (x - 1, y)
  Direita -> (x + 1, y)

