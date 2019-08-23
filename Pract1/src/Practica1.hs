-- | Estructuras Discretas 2020-1
-- | Práctica 1: Listas
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:

module Practica1 where

import Data.Char

-- | palindromo. Función que nos indica si una cadena es un palíndromo.
--
-- --> palindromo "hola" = False
-- --> palindromo "oso" = True
palindromo :: String -> Bool
palindromo word = word == reverse word

-- | minMax. Obtiene el elemento máximo y mínimo de una lista.
--
-- --> minMax [1,2,3,4,5] = (1,5)
-- --> minMax "hola" = ('a','o')
minMax :: Ord a => [a] -> (a,a)
minMax list = (minimum list, maximum list)

-- | rotar. Obtiene la lista obtenida poniendo los primeros n elementos al final de la lista.
--
-- --> rotar 3 [5,6,7,8,9] = [8,9,5,6,7]
-- --> rotar 2 "hola" = "laho"
rotar :: Int -> [a] -> [a]
rotar n list = drop n list ++ take n list

-- | atN. Devuelve el elemento en la n-ésima posición de la lista.
--
-- --> atN [1,2,4,6,7] 3 = 4
atN :: [a] -> Int -> a
atN list n = list !! (n - 1)

-- | selectMin. Selecciona los primeros k elementos de una lista, siendo k el elemento más pequeño de la lista.
--
-- --> selectMin [2,4,6,2,1,9] = [2]
selectMin :: [Int] -> [Int]
selectMin list = take (minimum list) list

-- | delN. Función que elimina el n-ésimo elemento de una lista
--
-- --> delN 4 [3,6,7,2,6,72] = [3,6,7,6,72]
delN :: Int -> [a] -> [a]
delN n list = (take (n - 1) list) ++ (drop n list)

-- | dIntervalos. Función que elimina los elementos de un intervalo de una lista.
--
-- --> dIntervalos 2 4 [1,2,3,4,5,6,7] = [1,5,6,7]
dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos min max list = (take (min - 1) list) ++ (drop max list)

-- | avgLen. Función que nos dice si el promedio de los elementos es menor que la longitud de la lista que los contiene
--
-- --> avgLen [1,2,3,14] = False
avgLen :: [Int] -> Bool
avgLen list = ((sum list) `div` length list) < length list

-- | primeros. Recibe una lista de duplas y regresa la lista con el primer elemento de cada par.
--
-- --> primeros [(1,2),(3,4),(5,6),(7,8)] = [1,3,5,7]
primeros :: [(a,b)] -> [a]
primeros list = [a | (a, _) <- list]

-- | segundos. Recibe una lista de duplas y regresa la lista con el segundo elemento de cada par.
--
-- --> segundos [(1,2),(3,4),(5,6),(7,8)] = [2,4,6,8]
segundos :: [(a,b)] -> [b]
segundos list = [b | (_, b) <- list]

-- | divN. Regresa una lista con todos los números divisibles entre n y 100, con n menor que 100
--
-- --> divN 13 = [13,26,39,52,65,78,91]
divN :: Int -> [Int]
divN n = [x | x <- [n..100], x `mod` n == 0]
-- divN n = [x*n | x <- [1..100], x*n < 100]

-- | minusculas. Convierte todas las letras de un String en minúsculas.
--
-- --> minusculas "HOLA" = "hola"
-- --> minusculas "DiScReTas" = "discretas"
minusculas :: String -> String
minusculas word = [toLower letter | letter <- word]

-- | sumaCuadrados. Calcula la suma de los n primeros cuadrados.
--
-- --> sumaCuadrados 10 = 385
sumaCuadrados :: Int -> Int
sumaCuadrados n = sum [x^2 | x <- [0..n]]

-- | mPares. Regresa una lista con el resultado de la multiplicación entre los elementos de cada par.
--
-- --> mPares [(1,2),(3,4),(5,6)] = [2,12,30]
mPares :: Num a => [(a,a)] -> [a]
mPares list = [x*y | (x, y) <- list]

-- | desdobla. Regresa una sola lista con todos los elementos de las sublistas.
--
-- --> desdobla [[1,2],[3,4],[5,6]] = [1,2,3,4,5,6]
desdobla :: [[a]] -> [a]
desdobla list = [x | y <- list, x <- y]

-- | prodC. Regresa el producto cartesiano entre dos listas.
--
-- --> prodC [1,2,3] "dia" = [(1,'d'),(1,'i'),(1,'a'),(2,'d'),(2,'i'),(2,'a'),(3,'d'),(3,'i'),(3,'a')]
prodC :: [a] -> [b] -> [(a,b)]
prodC l1 l2 = [(x1, x2) | x1 <- l1, x2 <- l2]

-- | separa. Separa en dos los elementos de la lista de entrada, en una los primeros de cada dupla y en otra los segundos.
--
-- --> separa [(1,2),(3,4),(5,6)] = ([1,3,5],[2,4,6])
separa :: [(a,b)] -> ([a],[b])
separa list = ([a | (a, _) <- list], [b | (_, b) <- list])

-- | serieGauss. Función que obtiene la lista que representa
-- | los primeros n elementos de la serie de Gauss.
--
-- --> serieGauss 10 = [0,1,3,6,10,15,21,28,36,45,55]
-- --> serieGauss 20 = [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210]
serieGauss :: Int -> [Int]
serieGauss n = [(x * (x + 1)) `div` 2 | x <- [0..n]]

-- | primos. Regresa los números primos de 1 hasta n
--
-- --> primos 100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
primos :: Int -> [Int]
primos n = [x | x <- [2..n], (length [y | y <- [2..x], x `mod` y == 0]) < 2]

