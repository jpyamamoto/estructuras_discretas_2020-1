-- | Estructuras Discretas 2020-1
-- | Semanal 1: Introducción a Haskell
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:

module Semanal1 where


-- | media3. Función que recibe tres parámetros x,y,z y regresa la media aritmética de ellos.
--
-- --> media3 (4,3,5)
-- --> media3 (2.0,5,8)
media3 :: (Float,Float,Float) -> Float
media3 (a,b,c) = (a + b + c) / 3

-- | absoluto. Función que regresa el valor absoluto de un entero.
--
-- --> absoluto 10
-- --> absoluto -18
absoluto :: Int -> Int
absoluto n = if n < 0 then -n else n

-- | distanciaPuntos. Función que regresa la distancia entre dos puntos.
--
-- --> distanciaPuntos (2,4) (-1,2)
-- --> distanciaPuntos (3,-2) (2.0,-3)
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt (((c - a) ** 2) + ((d - b) ** 2))

-- | sumaCom. Función que realiza la suma de dos números complejos.
--
-- --> sumaCom (1,2) (14,1)
-- --> sumaCom (3,1) (5,1)
sumaCom :: (Double,Double) -> (Double,Double) -> (Double,Double)
sumaCom (a,b) (c,d) = (a + c, b + d)

-- | mulCom. Función que realiza la multiplicación de dos números complejos.
--
-- --> mulCom (1,2) (14,1)
-- --> mulCom (3,1) (5,1)
mulCom :: (Double,Double) -> (Double,Double) -> (Double,Double)
mulCom (a,b) (c,d) = ((a * c) - (b * d), (a * d) + (b * c))

-- | esPar. Función que determina si un número es par.
--
-- --> esPar 4
-- --> esPar 15
esPar :: Int -> Bool
esPar n = (n `mod` 2) == 0

-- | areaCirculo. Función que determina el área de un círculo dado su radio.
--
-- --> areaCirculo 4
-- --> areaCirculo 1
areaCirculo :: Float -> Float
areaCirculo n = pi * (n ** 2)

-- | rex. Función que determina si Rex sale a jugar o no.
--
-- --> rex 25 False
-- --> rex 22 True
rex :: Int -> Bool -> String
rex n True = if n > 12 && n < 20 then "Sale a jugar" else "No sale a jugar"
rex n False = if n > 21 && n < 28 then "Sale a jugar" else "No sale a jugar"

-- | sumaGauss. Función que representa la fórmula de Gauss.
--
-- --> sumaGauss 10
-- --> sumaGauss 24
sumaGauss :: Int -> Int
sumaGauss n = (n * (n + 1)) `div` 2

-- | calculadora. Función que realiza operaciones aritméticas.
--
-- --> calculadora "pow" (2,8)
-- --> calculadora "last" (365,75)
calculadora :: String -> (Int,Int) -> Int
calculadora "first" (a,b) = a
calculadora "last" (a,b) = b
calculadora "sum" (a,b) = a + b
calculadora "subs" (a,b) = a - b
calculadora "mul" (a,b) = a * b
calculadora "div" (a,b) = a `div` b
calculadora "pow" (a,b) = a ^ b


--Ejemplos

media3_1 = media3 (4,3,5)
--Resultado: 4.0
media3_2 = media3 (2.0,5,8)
--Resultado: 5.0

absoluto1 = absoluto 10
--Resultado: 10
absoluto2 = absoluto (-18)
--Resultado: 18

distancia1 = distanciaPuntos (2,4) (-1,2)
--Resultado: 3.6055512
distancia2 = distanciaPuntos (3,-2) (2.0,-3)
--Resultado: 1.4142135

sumaCom1 = sumaCom (1,2) (14,1)
--Resultado: (15.0,3.0)
sumaCom2 = sumaCom (3,1) (5,1)
--Resultado: (8.0,2.0)

mulCom1 = mulCom (1,2) (14,1)
--Resultado: (12.0,29.0)
mulCom2 = mulCom (3,1) (5,1)
--Resultado: (14.0,8.0)

esPar1 = esPar 4
--Resultado: True
esPar2 = esPar 15
--Resultado: False

areaCirculo1 = areaCirculo 4
--Resultado: 50.265484
areaCirculo2 = areaCirculo 1
--Resultado: 3.1415927

rex1 = rex 25 False
--Resultado: "Sale a jugar"
rex2 = rex 22 True
--Resultado: "No sale a jugar"

sumaGauss1 = sumaGauss 112
--Resultado: 6328
sumaGauss2 = sumaGauss 1822
--Resultado: 1660753

calculadora1 = calculadora "pow" (2,8)
--Resultado: 256
calculadora2 = calculadora "last" (365,75)
--Resultado: 75
