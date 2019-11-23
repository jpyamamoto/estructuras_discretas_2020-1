-- | Estructuras Discretas 2020-1
-- | Práctica 5: Gráficas y Relaciones
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Practica5 where

type Graph a = ([a], [(a, a)])

-- | ingrado. Regresa el ingrado de un vértice.
--
-- --> ingrado 1 ([1,2,3,4], [(1,3),(2,1),(2,4),(3,2)]) = 1
ingrado :: Eq a => a -> Graph a -> Int
ingrado x (_, ys) = ingradoAux x ys
  where ingradoAux x []          = 0
        ingradoAux x ((_, b):ys) = if x == b then 1 + ingradoAux x ys else ingradoAux x ys

-- | exgrado. Regresa el exgrado de un vértice.
--
-- --> exgrado 2 ([1,2,3,4], [(1,3),(2,1),(2,4),(3,2)]) = 2
exgrado :: Eq a => a -> Graph a -> Int
exgrado x (_, ys) = exgradoAux x ys
  where exgradoAux x []          = 0
        exgradoAux x ((a, _):ys) = if x == a then 1 + exgradoAux x ys else exgradoAux x ys

-- | trayectorias. Regresa una lista con todas las trayectorias con el vértice recibido como inicial.
--
-- --> trayectorias 2 ([1,2,3,4], [(1,3),(2,1),(2,4),(3,2)]) = [[(2,1)],[(2,4)],[(2,1),(1,3)],[(2,1),(1,3),(3,2)]]
trayectorias :: Eq a => a -> Graph a -> [[(a, a)]]
trayectorias x (_, ys) = construyeCamino x [] ys

-- | esHamiltoniana. Indica si una digráfica es Hamiltoniana.
--
-- --> esHamiltoniana ([1,2,3,4], [(1,3),(2,1),(2,4),(3,2)]) = False
-- --> esHamiltoniana ([1,2,3,4], [(1,3),(2,1),(4,2),(3,4)]) = True
esHamiltoniana :: Eq a => Graph a -> Bool
esHamiltoniana (xs, edges) = esHamiltonianaAux (trayectorias (head xs) (xs, edges))
  where esHamiltonianaAux []     = False
        esHamiltonianaAux (y:ys) = (length y == length xs) || esHamiltonianaAux ys

-- | esReflexiva. Indica si una gráfica es reflexiva.
--
-- --> esReflexiva ([1,2,3,4,5], [(1,3),(1,4),(2,2),(2,5)]) = False
-- --> esReflexiva ([1,2,3,4,5], [(1,1),(2,2),(3,3),(4,4),(5,5)]) = True
esReflexiva :: Eq a => Graph a -> Bool
esReflexiva ([], _)    = True
esReflexiva (x:xs, ys) = if (elem (x, x) ys) then esReflexiva (xs, ys) else False

-- | esSimetrica. Indica si una gráfica es simétrica.
--
-- --> esSimetrica ([1,2,3,4,5], [(1,3),(1,4),(2,2),(2,5)]) = False
-- --> esSimetrica ([1,2,3,4,5], [(1,2),(2,1),(3,4),(4,3)]) = True
esSimetrica :: Eq a => Graph a -> Bool
esSimetrica (_, ys) = esSimetricaAux ys ys
  where esSimetricaAux [] _           = True
        esSimetricaAux ((a, b):xs) ys = if (elem (b, a) ys) then (esSimetricaAux xs ys) else False

-- | esAntisimetrica. Indica si una gráfica es simétrica.
--
-- --> esAntisimetrica ([1,2,3,4,5], [(1,3),(1,4),(2,2),(2,5)]) = True
-- --> esAntisimetrica ([1,2,3,4,5], [(1,2),(2,1),(3,4),(4,3)]) = False
esAntisimetrica :: Eq a => Graph a -> Bool
esAntisimetrica (_, xs) = esAntisimetricaAux xs xs
  where esAntisimetricaAux [] ys          = True
        esAntisimetricaAux ((a, b):xs) ys = if (a /= b) && (elem (b, a) ys) then False else esAntisimetricaAux xs ys

-- | cerrReflexiva. Regresa la cerradura reflexiva de la gráfica.
--
-- --> cerrReflexiva ([1,2,3,4,5], [(1,3),(1,4),(2,2),(2,5)]) = ([1,2,3,4,5],[(1,1),(3,3),(4,4),(5,5),(1,3),(1,4),(2,2),(2,5)])
-- --> cerrReflexiva ([1,2,3,4,5], [(1,1),(2,2),(3,3),(4,4),(5,5)]) = ([1,2,3,4,5],[(1,1),(2,2),(3,3),(4,4),(5,5)])
cerrReflexiva :: Eq a => Graph a -> Graph a
cerrReflexiva (xs, ys) = (xs, [(x, x) | x <- xs, not (elem (x, x) ys)] ++ ys)

-- | cerrSimetrica. Regresa la cerradura simétrica de la gráfica.
--
-- --> cerrSimetrica ([1,2,3,4,5], [(1,3),(1,4),(2,2),(2,5)]) = ([1,2,3,4,5],[(3,1),(4,1),(5,2),(1,3),(1,4),(2,2),(2,5)])
-- --> cerrSimetrica ([1,2,3,4,5], [(1,2),(2,1),(3,4),(4,3)]) = ([1,2,3,4,5],[(1,2),(2,1),(3,4),(4,3)])
cerrSimetrica :: Eq a => Graph a -> Graph a
cerrSimetrica (xs, ys) = (xs, [(y, x) | (x, y) <- ys, not (elem (y, x) ys)] ++ ys)

-- | composicion. Regresa la composición de dos gráficas.
--
-- --> composicion ([1,2,3,4,5], [(1,3),(1,4),(2,2),(2,5)]) ([1,2,3,4,5], [(1,2),(2,1),(3,4),(4,3)]) = ([1,2,3,4,5],[(1,4),(1,3),(2,1)])
composicion :: Eq a => Graph a -> Graph a -> Graph a
composicion (x, xs) (y, ys) = (x ++ [z | z <- y, not (elem z x)], [(a, d) | (a, b) <- xs, (c, d) <- ys, b == c])

-- | cerrTransitiva. Regresa la cerradura transitiva de la gráfica.
--
-- --> cerrTransitiva ([1,2,3,4,5], [(1,3),(1,4),(2,2),(2,5)]) = ([1,2,3,4,5],[(1,3),(1,4),(2,2),(2,5)])
-- --> cerrTransitiva ([1,2,3,4,5], [(1,3),(3,4),(2,4),(4,5)]) = ([1,2,3,4,5],[(1,3),(1,4),(1,5),(2,4),(2,5),(3,4),(3,5),(4,5)])
cerrTransitiva :: Eq a => Graph a -> Graph a
cerrTransitiva (xs, ys) = warshall (xs, ys) (length xs)

-- **************************
-- *                        *
-- *   Métodos Auxiliares   *
-- *                        *
-- **************************

-- | Utiliza el algoritmo de Warshall para generar la cerradura transitiva.
warshall :: Eq a => Graph a -> Int -> Graph a
warshall graph 0     = graph
warshall (xs, ys) n  =
  let preWarshall    = snd (warshall (xs, ys) (n-1))
      transitivo a b = elem (a, b) [(x, z) | x <- xs, y <- xs, z <- xs, elem (x, y) preWarshall && elem (y, z) preWarshall]
  in  (xs, [(a, b) | a <- xs, b <- xs, elem (a, b) preWarshall || transitivo a b])

-- | Construye los posibles caminos a partir de un vértice.
construyeCamino :: Eq a => a -> [(a, a)] -> [(a, a)] -> [[(a, a)]]
construyeCamino x cam rel =
  let caminos     = [cam ++ [next] | next <- sucesores x, not (elem next cam)]
      sucesores m = [(a, b) | (a, b) <- rel, a == m, a /= b, (visitado m) < 2]
      visitado f  = sum [1 | (a, b) <- cam, f == a || f == b]
  in caminos ++ concat [construyeCamino (snd (last camino)) camino rel | camino <- caminos]

