{-|
Module : Exemplo
Description : Módulo Haskell contendo funções recursivas.
Copyright : (c) Nelson <d12733@di.uminho.pt>, 2024
Maintainer : nelson@estevao.org2 

Este módulo contém definições Haskell para o cálculo de funções recursivas
simples.
-}
module Exemplo where

{-|
Função fatorial que recebe __um__ inteiro.

== Notas

* Esta função __não supporta numeros negativos__;
* O factorial de /zero/ é /um/;

== Mais Notas

1. Esta função __não supporta numeros negativos__;
1. O factorial de /zero/ é /um/;

== Exemplos

>>> fact 0
1

>>> fact 3
6
-}
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

{-|
Duplica um inteiro.
-}
duplica :: Int -> Int
duplica x = x * 2

-- Use comand haddock -h -o namefolder/html namefile 
-- Use comand cd namefolder/html && python3 -m http.server 4242 
-- In browser http://localhost:4242   