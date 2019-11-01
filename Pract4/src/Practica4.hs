-- | Estructuras Discretas 2020-1
-- | Práctica 4: Estructuras Recursivas Parte 2
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Practica4 where

data Nat = Zero | Suc Nat deriving(Show)

data BTree a = Void | Node (BTree a) a (BTree a) deriving(Show)

data SnocList a = Empty | Snoc (SnocList a) a deriving(Show)

-- | igualNat. Nos dice si dos números naturales son iguales.
--
-- --> igualNat
igualNat :: Nat -> Nat -> Bool
igualNat Zero Zero       = True
igualNat (Suc m) (Suc n) = igualNat m n
igualNat _ _             = False

-- | natToInt. Convierte un Nat a un Int.
--
-- --> natToInt
natToInt :: Nat -> Int
natToInt Zero   = 0
natToInt (Suc n) = 1 + natToInt n

-- | intToNat. Convierte un Int a un Nat.
--
-- --> intToNat
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = Suc (intToNat (x-1))

-- | restaNat. Resta dos números naturales.
--
-- --> restaNat
restaNat :: Nat -> Nat -> Nat
restaNat numero Zero     = numero
restaNat (Suc n) (Suc m) = restaNat n m

-- | preorden. Convierte un árbol binario a lista utilizando preorden.
--
-- --> preorden
preorden :: BTree a -> [a]
preorden Void         = []
preorden (Node i r d) = [r] ++ (preorden i) ++ (preorden d)

-- | inorden. Convierte un árbol binario a lista utilizando inorden.
--
-- --> inorden
inorden :: BTree a -> [a]
inorden Void         = []
inorden (Node i r d) = (inorden i) ++ [r] ++ (inorden d)

-- | postorden. Convierte un árbol binario a lista utilizando postorden.
--
-- --> postorden
postorden :: BTree a -> [a]
postorden Void         = []
postorden (Node i r d) = (postorden i) ++ (postorden d) ++ [r]

-- | agregaOrd. Agrega un elemento en un árbol binario ordenado, manteniendo
-- el órden.
--
-- --> agregaOrd
agregaOrd :: Ord a => a -> BTree a -> BTree a
agregaOrd x Void = (Node Void x Void)
agregaOrd x (Node i r d)
  | x == r = (Node i r (Node d x Void))
  | x < r  = (Node (agregaOrd x i) r d)
  | x > r  = (Node i r (agregaOrd x d))

-- | reflejo. Regresa el reflejo de un árbol binario.
--
-- --> reflejo
reflejo :: BTree a -> BTree a
reflejo Void         = Void
reflejo (Node i r d) = (Node (reflejo d) r (reflejo i))

-- | peso. Regresa la distancia del camino más largo de la raíz a alguna hoja.
--
-- --> peso
peso :: BTree a -> Int
peso Void         = 0
peso (Node i r d) = maximum([1 + (peso i), 1, 1 + (peso d)])

-- | size. Regresa la cantidad de elementos en un árbol.
--
-- --> size
size :: BTree a -> Int
size Void         = 0
size (Node i r d) = 1 + (size i) + (size d)

-- | lastSnoc. Devuelve el último elemento de una lista Snoc.
--
-- --> lastSnoc
lastSnoc :: SnocList a -> a
lastSnoc Empty = error "Lista vacía"
lastSnoc (Snoc _ x) = x

-- | takeSnoc. Devuelve los primeros n elementos de una lista Snoc.
--
-- --> takeSnoc
takeSnoc :: Int -> SnocList a -> SnocList a
takeSnoc x ls = takeSnocAux ((lengthSnoc ls) - x) ls
  where
    takeSnocAux _ Empty = Empty
    takeSnocAux x (Snoc ls l)
      | x <= 0    = (Snoc ls l)
      | otherwise = takeSnocAux (x - 1) ls

-- | dropSnoc. Devuelve una lista Snoc sin los primeros n elementos.
--
-- --> dropSnoc
dropSnoc :: Int -> SnocList a -> SnocList a
dropSnoc x ls = dropSnocAux ((lengthSnoc ls) - x) ls
  where
    dropSnocAux _ Empty = Empty
    dropSnocAux x (Snoc ls l)
      | x <= 0    = Empty
      | otherwise = (Snoc (dropSnocAux (x - 1) ls) l)

-- | reversaSnoc. Devuelve una lista Snoc en reversa.
--
-- --> reversaSnoc
reversaSnoc :: SnocList a -> SnocList a
reversaSnoc ls = reversaSnocAux ls Empty
  where
    reversaSnocAux Empty temp       = temp
    reversaSnocAux (Snoc ls l) temp = reversaSnocAux ls (Snoc temp l)

-- **************************
-- *                        *
-- *   Métodos Auxiliares   *
-- *                        *
-- **************************

-- | lengthSnoc. Regresa la longitud de una lista Snoc.
--
-- --> lengthSnoc
lengthSnoc :: SnocList a -> Int
lengthSnoc Empty          = 0
lengthSnoc (Snoc a _)     = 1 + lengthSnoc a
