-- | Estructuras Discretas 2020-1
-- | Práctica 5: Gráficas y Relaciones
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Practica5 where

type Graph a = ([a], [(a, a)])

ingrado :: Eq a => a -> Graph a -> Int
ingrado x (_, ys) = ingradoAux x ys
  where ingradoAux x []          = 0
        ingradoAux x ((_, b):ys) = if x == b then 1 + (ingradoAux x ys) else (ingradoAux x ys)

exgrado :: Eq a => a -> Graph a -> Int
exgrado x (_, ys) = exgradoAux x ys
  where exgradoAux x []          = 0
        exgradoAux x ((a, _):ys) = if x == a then 1 + (exgradoAux x ys) else (exgradoAux x ys)

esReflexiva :: Eq a => Graph a -> Bool
esReflexiva ([], _)    = True
esReflexiva (x:xs, ys) = if (esElemento (x, x) ys) then esReflexiva (xs, ys) else False

esSimetrica :: Eq a => Graph a -> Bool
esSimetrica (_, ys) = esSimetricaAux ys ys
  where esSimetricaAux [] _           = True
        esSimetricaAux ((a, b):xs) ys = if (esElemento (b, a) ys) then (esSimetricaAux xs ys) else False

esAntisimetrica :: Eq a => Graph a -> Bool
esAntisimetrica (_, xs) = esAntisimetricaAux xs xs
  where esAntisimetricaAux [] ys          = True
        esAntisimetricaAux ((a, b):xs) ys = if (a /= b) && (esElemento (b, a) ys) then False else esAntisimetricaAux xs ys

cerrReflexiva :: Eq a => Graph a -> Graph a
cerrReflexiva (xs, ys) = (xs, [(x, x) | x <- xs, not (esElemento (x, x) ys)] ++ ys)

cerrSimetrica :: Eq a => Graph a -> Graph a
cerrSimetrica (xs, ys) = (xs, [(y, x) | (x, y) <- ys, not (esElemento (y, x) ys)] ++ ys)

composicion :: Eq a => Graph a -> Graph a -> Graph a
composicion (x, xs) (y, ys) = (x ++ [z | z <- y, not (esElemento z x)], [(a, d) | (a, b) <- xs, (c, d) <- ys, b == c])

-- **************************
-- *                        *
-- *   Métodos Auxiliares   *
-- *                        *
-- **************************

-- | esElemento. Decide si un valor es un elemento de una lista.
--
-- --> esElemento 4 [1,2,3,4,5] = True
-- --> esElemento 9 [1,2,3,4,5] = False
esElemento :: Eq a => a -> [a] -> Bool
esElemento _ [] = False
esElemento y (x:xs) = if y /= x then esElemento y xs else True
