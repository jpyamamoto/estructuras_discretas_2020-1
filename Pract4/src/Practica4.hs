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
-- --> igualNat (Suc(Suc(Zero))) (Suc(Suc(Suc(Zero)))) = False
-- --> igualNat (Suc(Suc(Zero))) (Suc(Suc(Zero))) = True
igualNat :: Nat -> Nat -> Bool
igualNat Zero Zero       = True
igualNat (Suc m) (Suc n) = igualNat m n
igualNat _ _             = False

-- | natToInt. Convierte un Nat a un Int.
--
-- --> natToInt (Suc(Suc(Suc(Suc(Zero))))) = 4
-- --> natToInt (Suc(Suc(Suc(Suc(Suc(Suc(Zero))))))) = 6
natToInt :: Nat -> Int
natToInt Zero   = 0
natToInt (Suc n) = 1 + natToInt n

-- | intToNat. Convierte un Int a un Nat.
--
-- --> intToNat 0 = Zero
-- --> intToNat 5 = Suc (Suc (Suc (Suc (Suc Zero))))
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = Suc (intToNat (x-1))

-- | restaNat. Resta dos números naturales.
--
-- --> restaNat (Suc(Suc(Suc(Suc(Suc(Suc(Zero))))))) (Suc(Suc(Suc(Zero)))) = Suc (Suc (Suc Zero))
-- --> restaNat (Suc(Suc(Zero))) Zero = Suc (Suc Zero)
restaNat :: Nat -> Nat -> Nat
restaNat numero Zero     = numero
restaNat (Suc n) (Suc m) = restaNat n m

-- | preorden. Convierte un árbol binario a lista utilizando preorden.
--
-- --> preorden (Node (Node Void 1 Void) 2 (Node Void 3 Void)) = [2,1,3]
-- --> preorden (Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 Void) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)) = [8,3,1,6,4,7,10]
preorden :: BTree a -> [a]
preorden Void         = []
preorden (Node i r d) = [r] ++ (preorden i) ++ (preorden d)

-- | inorden. Convierte un árbol binario a lista utilizando inorden.
--
-- --> inorden (Node (Node Void 1 Void) 2 (Node Void 3 Void)) = [1,2,3]
-- --> inorden (Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 Void) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)) = [1,3,4,6,7,8,10]
inorden :: BTree a -> [a]
inorden Void         = []
inorden (Node i r d) = (inorden i) ++ [r] ++ (inorden d)

-- | postorden. Convierte un árbol binario a lista utilizando postorden.
--
-- --> postorden (Node (Node Void 1 Void) 2 (Node Void 3 Void)) = [1,3,2]
-- --> postorden (Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 Void) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)) = [1,4,7,6,3,10,8]
postorden :: BTree a -> [a]
postorden Void         = []
postorden (Node i r d) = (postorden i) ++ (postorden d) ++ [r]

-- | agregaOrd. Agrega un elemento en un árbol binario ordenado, manteniendo
-- el órden.
--
-- --> agregaOrd 5 (Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 Void) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)) = 
-- Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 (Node Void 5 Void)) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)
-- --> agregaOrd 2 (Node (Node Void 1 Void) 2 (Node Void 3 Void)) =
-- Node (Node Void 1 Void) 2 (Node Void 2 (Node Void 3 Void))
agregaOrd :: Ord a => a -> BTree a -> BTree a
agregaOrd x Void = (Node Void x Void)
agregaOrd x (Node i r d)
  | x == r = (Node i r (Node Void x d))
  | x < r  = (Node (agregaOrd x i) r d)
  | x > r  = (Node i r (agregaOrd x d))

-- | reflejo. Regresa el reflejo de un árbol binario.
--
-- --> reflejo (Node (Node Void 1 Void) 2 (Node Void 3 Void)) =
-- Node (Node Void 3 Void) 2 (Node Void 1 Void)
-- --> reflejo (Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 Void) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)) =
-- Node (Node Void 10 Void) 8 (Node (Node (Node Void 7 Void) 6 (Node Void 4 Void)) 3 (Node Void 1 Void))
reflejo :: BTree a -> BTree a
reflejo Void         = Void
reflejo (Node i r d) = (Node (reflejo d) r (reflejo i))

-- | peso. Regresa la distancia del camino más largo de la raíz a alguna hoja.
--
-- --> peso (Node (Node Void 1 Void) 2 (Node Void 3 Void)) = 2
-- --> peso (Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 Void) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)) = 4
peso :: BTree a -> Int
peso Void         = 0
peso (Node i r d) = maximum([1 + (peso i), 1, 1 + (peso d)])

-- | size. Regresa la cantidad de elementos en un árbol.
--
-- --> size (Node (Node Void 1 Void) 2 (Node Void 3 Void)) = 3
-- --> size (Node (Node (Node Void 1 Void) 3 (Node (Node Void 4 Void) 6 (Node Void 7 Void))) 8 (Node Void 10 Void)) = 7
size :: BTree a -> Int
size Void         = 0
size (Node i r d) = 1 + (size i) + (size d)

-- | lastSnoc. Devuelve el último elemento de una lista Snoc.
--
-- --> lastSnoc (Snoc (Snoc (Snoc Empty 1) 2) 3) = 3
-- --> lastSnoc (Snoc(Snoc(Snoc (Snoc (Snoc Empty 1) 2) 3) 4) 5) = 5
lastSnoc :: SnocList a -> a
lastSnoc Empty = error "Lista vacía"
lastSnoc (Snoc _ x) = x

-- | takeSnoc. Devuelve los primeros n elementos de una lista Snoc.
--
-- --> takeSnoc 0 (Snoc (Snoc (Snoc Empty 1) 2) 3) = Empty
-- --> takeSnoc 2 (Snoc (Snoc (Snoc Empty 1) 2) 3) = Snoc (Snoc Empty 1) 2
takeSnoc :: Int -> SnocList a -> SnocList a
takeSnoc x ls = takeSnocAux ((lengthSnoc ls) - x) ls
  where
    takeSnocAux _ Empty = Empty
    takeSnocAux x (Snoc ls l)
      | x <= 0    = (Snoc ls l)
      | otherwise = takeSnocAux (x - 1) ls

-- | dropSnoc. Devuelve una lista Snoc sin los primeros n elementos.
--
-- --> dropSnoc 0 (Snoc (Snoc (Snoc Empty 1) 2) 3) = Snoc (Snoc (Snoc Empty 1) 2) 3
-- --> dropSnoc 1 (Snoc (Snoc (Snoc Empty 1) 2) 3) = Snoc (Snoc Empty 2) 3
dropSnoc :: Int -> SnocList a -> SnocList a
dropSnoc x ls = dropSnocAux ((lengthSnoc ls) - x) ls
  where
    dropSnocAux _ Empty = Empty
    dropSnocAux x (Snoc ls l)
      | x <= 0    = Empty
      | otherwise = (Snoc (dropSnocAux (x - 1) ls) l)

-- | reversaSnoc. Devuelve una lista Snoc en reversa.
--
-- --> reversaSnoc Empty = Empty
-- --> reversaSnoc (Snoc (Snoc (Snoc Empty 1) 2) 3) = Snoc (Snoc (Snoc Empty 3) 2) 1
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
