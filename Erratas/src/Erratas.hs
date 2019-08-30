-- | Estructuras discretas 2020-1
-- | Ejercicio Erratas
-- | Profesora: M. en C. Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Erratas where

data L = A | J | Q | K deriving(Show, Eq)

data N = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 deriving(Show, Eq)

data F = Trebol | Corazon | Diamante | Espada deriving(Show, Eq)

data S = CartaT1 F N | CartaT2 F L | Comodin deriving(Show,Eq)

type ManoDos = (S,S)

-- | hazManoDos. Hace una dupla de dos manos ingresando los valores de las cartas.
--
-- --> hazManoDos (CartaT1 Trebol N2) (CartaT2 Corazon A) = (CartaT1 Trebol N2,CartaT2 Corazon A)
hazManoDos :: S -> S -> ManoDos
hazManoDos c1 c2 = (c1,c2)

type ManoT = (S,S,S)

-- | obtenCarta1. Regresa la primera carta de una mano de tres.
--
-- --> obtenCarta1 (CartaT1 Corazon N2, CartaT2 Trebol A, CartaT1 Diamante N4) = CartaT1 Corazon N2
obtenCarta1 :: ManoT -> S
obtenCarta1 (c1,_,_) = c1

-- | obtenCarta2. Regresa la segunda carta de una mano de tres.
--
-- --> obtenCarta2 (CartaT1 Corazon N2, CartaT2 Trebol A, CartaT1 Diamante N4) = CartaT2 Trebol A
obtenCarta2 :: ManoT -> S
obtenCarta2 (_,c2,_) = c2

-- | obtenCarta3. Regresa la tercera carta de una mano de tres.
--
-- --> obtenCarta3 (CartaT1 Corazon N2, CartaT2 Trebol A, CartaT1 Diamante N4) = CartaT1 Diamante N4
obtenCarta3 :: ManoT -> S
obtenCarta3 (_,_,c3) = c3

-- | manoTresEqMano. Compara si dos manos de tres cartas son iguales.
--
-- --> manoTresEqMano (CartaT1 Corazon N2, CartaT2 Trebol A, CartaT1 Diamante N4) (CartaT2 Espada K, CartaT1 Corazon N9, CartaT1 Trebol N6) = False
-- --> manoTresEqMano (CartaT1 Corazon N2, CartaT2 Trebol A, CartaT1 Diamante N4) (CartaT1 Corazon N2, CartaT2 Trebol A, CartaT1 Diamante N4) = True
manoTresEqMano :: ManoT -> ManoT -> Bool
manoTresEqMano (c11,c12,c13) (c21,c22,c23) = (c11 == c21) && (c12 == c22) && (c13 == c23)

data R2 = Punto Double Double

instance Show R2 where
  show (Punto x1 x2) = "("++show x1++","++show x2++")"

-- | v1. Regresa la coordenada x de un punto.
--
-- --> v1 (Punto 2.0 3.0) = 2.0
v1 :: R2 -> Double
v1 (Punto x y) = x

-- | v2. Regresa la coordenada y de un punto.
--
-- --> v2 (Punto 2.0 3.0) = 3.0
v2 :: R2 -> Double
v2 (Punto x y ) = y

-- | v. Regresa la diferencia entre las coordenadas de dos puntos.
--
-- --> v (Punto 2.0 3.0) (Punto 5.0 8.0) = (3.0,5.0)
v :: R2 -> R2 -> R2
v p1 p2 = let x = (v1 p2)-(v1 p1) in Punto x y where
  y = (v2 p2)-(v2 p1)

data Recta = Igual Double Double Double Double

instance Show Recta where
  show (Igual x v1 y v2) = "(x-"++show x++"/"++show v1++") = (y-"++show y++"/"++show v2++")"

-- | recta. Regresa la ecuación de una recta que se crea a partir de dos puntos.
--
-- --> recta (Punto 5.0 2.0) (Punto 10.0 6.0) = (x-5.0/5.0) = (y-6.0/4.0)
recta :: R2 -> R2 -> Recta
recta p1 p2 = let x1 = v1 p1 in let y1 = v2 p2 in Igual x1 x21 y1 y21 where
  vd = v p1 p2
  x21 = v1 vd
  y21 = v2 vd
