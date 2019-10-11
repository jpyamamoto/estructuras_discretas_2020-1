-- | Estructuras Discretas 2020-1
-- | Semanal 2: Estructuras Recursivas
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Semanal2 where

data Nat = Zero | Suc Nat deriving(Show)

data BTree a = Void | Node (BTree a) a (BTree a) deriving(Show)

data SnocList a = Empty | Snoc (SnocList a) a deriving(Show)

-- | sumaNat. Realiza la suma de dos números naturales.
--
-- --> sumaNat
sumaNat :: Nat -> Nat -> Nat
sumaNat a Zero    = a
sumaNat a (Suc b) = sumaNat (Suc a) b

-- | multNat. Realiza la multiplicación de dos números naturales.
--
-- --> multNat
multNat :: Nat -> Nat -> Nat
multNat a Zero       = Zero
multNat Zero a       = Zero
multNat a (Suc Zero) = a
multNat a (Suc b)    = multNat (sumaNat a a) b

-- | menorNat. Decide si a es menor que b.
--
-- --> menorNat
menorNat :: Nat -> Nat -> Bool
menorNat Zero b          = True
menorNat a Zero          = False
menorNat (Suc a) (Suc b) = menorNat a b

-- | mayorNat. Decide si a es mayor que b.
--
-- --> mayorNat
mayorNat :: Nat -> Nat -> Bool
mayorNat Zero b          = False
mayorNat a Zero          = True
mayorNat (Suc a) (Suc b) = mayorNat a b

-- | inTree. Decide si un elemento está en un árbol.
--
-- --> inTree
inTree :: Ord a => a -> BTree a -> Bool
inTree a Void         = False
inTree a (Node i c d) = if a == c then True else (inTree a i) || (inTree a d)

-- | maximo. Regresa el elemento con mayor valor de un árbol.
--
-- --> maximo
maximo :: Ord a => BTree a -> a
maximo (Node Void c Void) = c
maximo (Node i c Void)    = maximum [(maximo i), c]
maximo (Node Void c d)    = maximum [c, (maximo d)]
maximo (Node i c d)       = maximum [(maximo d), c, (maximo i)]

-- | minimo. Regresa el elemento con minimo valor de un árbol.
--
-- --> minimo
minimo :: Ord a => BTree a -> a
minimo (Node Void c Void) = c
minimo (Node i c Void)    = minimum [(minimo i), c]
minimo (Node Void c d)    = minimum [c, (minimo d)]
minimo (Node i c d)       = minimum [(minimo d), c, (minimo i)]

