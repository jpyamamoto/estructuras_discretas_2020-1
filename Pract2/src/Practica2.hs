-- | Estructuras Discretas 2020-1
-- | Práctica 2: Recursión sobre listas
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Practica2 where

-- | fibonacci. Regresa el n-ésimo elemento de la serie de fibonacci.
--
-- --> fibonacci 8 = 21
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- | and'. Determina si todos los valores en la lista son verdaderos.
--
-- --> and' [True, True, True] = True
-- --> and' [True, True, False] = False
and' :: [Bool] -> Bool
and' [] = True
and' (False:xs) = False
and' (x:xs) = and' xs

-- | concat'. Regresa las listas dentro de una lista, concatenadas.
--
-- --> concat' [[1,2,3], [4,5,6]] = [1,2,3,4,5,6]
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat'(xs)

-- | esElemento. Decide si un valor es un elemento de una lista.
--
-- --> esElemento 4 [1,2,3,4,5] = True
-- --> esElemento 9 [1,2,3,4,5] = False
esElemento :: Eq a => a -> [a] -> Bool
esElemento _ [] = False
esElemento y (x:xs) = if y /= x then esElemento y xs else True

-- | merge. Concatena dos listas ordenadas, en una sola lista ordenada.
--
-- --> merge [3,5,2] [9,1,6] = [1,2,3,5,6,9]
merge :: Ord a => [a] -> [a] -> [a]
merge lista1 lista2 = ordenar (unir lista1 lista2)
  where unir [] ys = ys
        unir (x:xs) ys = x:(unir xs ys)

-- | ordenar. Función auxiliar para ordenar una lista de menor a mayor.
--
-- --> ordenar [3,5,2,9,1,6] = [1,2,3,5,6,9]
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = insert x (ordenar xs)
  where insert p [] = [p]
        insert p (q:rs) = if p <= q then p:q:rs else q:(insert p rs)

-- | inversa. Intercambia de lugar los elementos de una lista de pares.
--
-- --> inversa [(1,2),(3,4),(5,6)] = [(2,1),(4,3),(6,5)]
inversa :: [(a,b)] -> [(b,a)]
inversa [] = []
inversa ((a,b):xs) = (b,a):(inversa xs)

-- | eliminaElem. Elimina todas las ocurrencias de un elemento x en una lista.
--
-- --> eliminaElem 1 [2,6,1,8,9,1,4] = [2,6,8,9,4]
eliminaElem :: Eq a => a -> [a] -> [a]
eliminaElem _ [] = []
eliminaElem x (y:ys) = if x == y then eliminaElem x ys else y:(eliminaElem x ys)

-- | eliminaRep. Elimina los elementos repetidos de una lista.
--
-- --> eliminaRep [1,2,3,4,5,6,1,2,3] = [1,2,3,4,5,6]
eliminaRep :: Eq a => [a] -> [a]
eliminaRep xs = auxEliminaRep xs []
  where auxEliminaRep [] _ = []
        auxEliminaRep (x:xs) ys = if esElemento x ys then auxEliminaRep xs ys else x:(auxEliminaRep xs (x:ys))

-- | cuenta. Cuenta cuántas veces aparece un elemento x en una lista.
--
-- --> cuenta 1 [1,2,3,1,5,6] = 2
cuenta :: Eq a => a -> [a] -> Int
cuenta y [] = 0
cuenta y (x:xs) = if y == x then 1 + (cuenta y xs) else cuenta y xs

-- | frecuencia. Regresa una lista de pares (x,y) donde x es un elemento y y es la cantidad de veces que aparece en la lista.
--
-- --> frecuencia [1,2,2,3,3,3,4,4,4,4,4] = [(1,1),(2,2),(3,3),(4,5)]
frecuencia :: Eq a => [a] -> [(a, Int)]
frecuencia xs = auxFrecuencia xs []
  where auxFrecuencia [] _ = []
        auxFrecuencia (x:xs) ys = if esElemento x ys then auxFrecuencia xs ys else (x, cuenta x (x:xs)):(auxFrecuencia xs (x:ys))
