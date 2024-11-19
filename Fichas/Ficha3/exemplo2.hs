{-|
Module : Exemplo
Description : Módulo Haskell contendo funções recursivas.
Copyright : (c) Alguém <alguem@algures.com>, 2024
            Outro Alguém <outro@algures.com>, 2024
Maintainer : alguem@algures.com
Stability : experimental
Portability : POSIX

Este módulo contém definições Haskell para o cálculo de funções recursivas simples (obs: isto é apenas uma descrição mais longa do módulo para efeitos de documentação.)
-}

module Exemplo where

{-| A função ’fact’ calcula o factorial (@fact n@ retorna o factorial de um inteiro @n@).

Alternativamente a função poderia ser definida da seguinte forma:

@
fact n = if n>0 then n * fact (n-1)
else 1

@

== __Exemplos de utilização:__
>>> fact 5
120
>>> fact 0
1

== __Propriedades:__
prop> fact 0 = 1
prop> n>0 => fact n = n * fact (n-1)
-}
fact ::
    Integer  -- ^ argumento assume-se não negativo
 -> Integer  -- ^ resultado
fact 0 = 1
fact n = n * fact (n-1)

{-| A função ’fib’ retorna o @n@-ésimo elemento da sequência de Fibonacci.
Algumas características que distinguem a função ’fib’ da ’fact’:

* o padrão de recursividade é binário;

* dispõe de dois casos base.

== __Exemplos de utilização:__
>>> fib 4
3
>>> fib 10
55
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
