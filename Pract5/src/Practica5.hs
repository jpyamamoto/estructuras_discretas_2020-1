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
        ingradoAux x ((_, b):ys) = if x == b then 1 + ingradoAux x ys else ingradoAux x ys

exgrado :: Eq a => a -> Graph a -> Int
exgrado x (_, ys) = exgradoAux x ys
  where exgradoAux x []          = 0
        exgradoAux x ((a, _):ys) = if x == a then 1 + exgradoAux x ys else exgradoAux x ys

trayectorias :: Eq a => a -> Graph a -> [[(a, a)]]
trayectorias x (_, ys) = construyeCamino x [] ys

esReflexiva :: Eq a => Graph a -> Bool
esReflexiva ([], _)    = True
esReflexiva (x:xs, ys) = if (elem (x, x) ys) then esReflexiva (xs, ys) else False

esSimetrica :: Eq a => Graph a -> Bool
esSimetrica (_, ys) = esSimetricaAux ys ys
  where esSimetricaAux [] _           = True
        esSimetricaAux ((a, b):xs) ys = if (elem (b, a) ys) then (esSimetricaAux xs ys) else False

esAntisimetrica :: Eq a => Graph a -> Bool
esAntisimetrica (_, xs) = esAntisimetricaAux xs xs
  where esAntisimetricaAux [] ys          = True
        esAntisimetricaAux ((a, b):xs) ys = if (a /= b) && (elem (b, a) ys) then False else esAntisimetricaAux xs ys

cerrReflexiva :: Eq a => Graph a -> Graph a
cerrReflexiva (xs, ys) = (xs, [(x, x) | x <- xs, not (elem (x, x) ys)] ++ ys)

cerrSimetrica :: Eq a => Graph a -> Graph a
cerrSimetrica (xs, ys) = (xs, [(y, x) | (x, y) <- ys, not (elem (y, x) ys)] ++ ys)

composicion :: Eq a => Graph a -> Graph a -> Graph a
composicion (x, xs) (y, ys) = (x ++ [z | z <- y, not (elem z x)], [(a, d) | (a, b) <- xs, (c, d) <- ys, b == c])

cerrTransitiva :: Eq a => Graph a -> Graph a
cerrTransitiva (xs, ys) = warshall (xs, ys) (length xs)

-- **************************
-- *                        *
-- *   Métodos Auxiliares   *
-- *                        *
-- **************************

warshall :: Eq a => Graph a -> Int -> Graph a
warshall graph 0     = graph
warshall (xs, ys) n  =
  let preWarshall    = snd (warshall (xs, ys) (n-1))
      transitivo a b = elem (a, b) [(x, z) | x <- xs, y <- xs, z <- xs, elem (x, y) preWarshall && elem (y, z) preWarshall]
  in  (xs, [(a, b) | a <- xs, b <- xs, elem (a, b) preWarshall || transitivo a b])

construyeCamino :: Eq a => a -> [(a, a)] -> [(a, a)] -> [[(a, a)]]
construyeCamino x cam rel =
  let caminos = [cam ++ [next] | next <- sucesores x rel, not (elem next cam)]
      visitado f = sum [1 | (a, _) <- cam, f /= a]
      sucesores m ns = [(a, b) | (a, b) <- ns, a == m, a /= b, (visitado a) <= 1]
  in caminos ++ concat [construyeCamino (snd (last camino)) camino rel | camino <- caminos]

