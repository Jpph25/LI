module Snake where  

data Direction = Cima 
               | Baixo 
               | Esquerda 
               | Direita 
               deriving (Show,Eq,Ord) 

type Points = (Float,Float) 

data Cobra = Cobra Direction [Points] 
           deriving (Show,Eq) 

atualiza :: Cobra -> Cobra 
atualiza (Cobra direction pontos) 
        = Cobra direction novosPontos 
    where 
          novosPontos = map (\p -> atualizPonto direction p) pontos     

atualizPonto :: Direction -> Points -> Points 
atualizPonto direction (x,y) 
            = case direction of 
              Cima     -> (x, y + 1) 
              Baixo    -> (x, y - 1) 
              Esquerda -> (x - 1, y)
              Direita  -> (x + 1, y) 