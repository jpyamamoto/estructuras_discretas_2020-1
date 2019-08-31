-- | Estructuras Discretas 2020-1
-- | Ejercicios 30/agosto/2019
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Ejercicios where

import Data.List

-- | reverse. Función que regresa una lista en reversa.
--
-- --> reverse [1,2,3] = [3,2,1]
reversa :: Num a => [a] -> [a]
reversa [] = []
reversa (n:ns) = (reversa ns) ++ [n]

-- | concatena. Concatena dos listas.
--
-- --> concatena [1,2,3] [4,5,6] = [1,2,3,4,5,6]
concatena :: Num a => [a] -> [a] -> [a]
concatena lista1 lista2 = aux_concatena (reversa lista1) lista2

aux_concatena :: Num a => [a] -> [a] -> [a]
aux_concatena [] lista = lista
aux_concatena (n:ns) lista = aux_concatena ns (n:lista)

-- | concatena [] ys = ys
-- | concatena (x:xs) = (x:(concatena xs ys))

-- | insert. Inserta un elemento en una lista en la n-ésima posición.
--
-- --> insert 1 3 [4,3,2] = [4,3,2,1]
insert :: Int -> Int -> [Int] -> [Int]
insert a b lista = aux_insert a b (reversa lista) []

aux_insert :: Int -> Int -> [Int] -> [Int] -> [Int]
aux_insert a 0 lista1 lista2 = lista2 ++ [a] ++ lista1
aux_insert a b (n:ns) lista2 = aux_insert a (b-1) ns (n:lista2)

-- | sum. Suma los elementos de una lista.
--
-- --> sum [1,2,3] = 6
suma :: [Int] -> Int
suma [] = 0
suma (n:ns) = n + suma ns

-- | isort
--
-- --> isort [3,7,4,8,9] = [3,4,7,8,9]
isort :: [Int] -> [Int]
isort [] = []
isort lista = min:(isort (delete min lista))
  where min = minimum lista
