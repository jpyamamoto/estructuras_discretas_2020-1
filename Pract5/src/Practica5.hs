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

