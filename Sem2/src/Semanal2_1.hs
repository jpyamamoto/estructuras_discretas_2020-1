
-- | Estructuras Discretas 2020-1
-- | Semanal 2: Introducción a Haskell
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes: Juan Pablo Yamamoto Zazueta y Valeria Reyes Tapia.

module Semanal2 where
data Nat = Zero | Suc Nat deriving Show
data BTree a = Void | Node (BTree a) a (BTree a) deriving Show
data SnocList a = Empty | Snoc (SnocList a) a deriving Show

-- | sumaNat. Define la función sumaNat que realiza la suma de dos números naturales.
--
-- --> sumaNat (Suc(Suc(Suc Zero))) (Suc(Suc Zero)) = Suc (Suc (Suc (Suc (Suc Zero))))
-- --> sumaNat Zero (Suc(Suc Zero)) = Suc (Suc Zero)
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero    x = x
sumaNat (Suc x) y = Suc (sumaNat x y)

-- | multNat.Define la función multNat que realiza la multiplicación de dos números naturales.
--
-- --> multNat Zero (Suc(Suc Zero)) = Zero
-- --> multNat (Suc(Suc Zero)) (Suc(Suc Zero)) = Suc (Suc (Suc (Suc Zero)))
multNat :: Nat -> Nat -> Nat
multNat Zero _    = Zero
multNat (Suc m) n = sumaNat n (multNat m n)

-- | menorNat.Define la función menorNat que dados dos números naturales a y b, nos dice si a es menor que b.
--
-- --> menorNat (Suc(Suc(Suc Zero))) (Suc(Suc Zero)) = False
-- --> menorNat (Suc Zero) (Suc(Suc Zero)) = True
menorNat :: Nat -> Nat -> Bool
menorNat a Zero = False
menorNat Zero b = True
menorNat (Suc a) (Suc b) = menorNat a b


-- | mayorNat.Define la función mayorNat que dados dos números naturales a y b, nos dice si a es mayor que b.
--
-- --> mayorNat (Suc(Suc(Suc Zero))) (Suc(Suc Zero)) = True
-- --> mayorNat (Suc Zero) (Suc(Suc Zero)) = False
mayorNat :: Nat -> Nat -> Bool
mayorNat (Suc x) Zero = True
mayorNat (Suc x) (Suc y) = mayorNat x y
mayorNat _ _ = False

-- | inTree.Define la función inTree que dados un elemento e y un árbol T, nos dice si e está en T.
--
-- --> inTree 4 (Node (Node Void 2 Void) 1 (Node (Node Void 4 Void) 3 (Node Void 5 Void))) = True
-- --> inTree 0 (Node (Node Void 2 Void) 1 (Node (Node Void 4 Void) 3 (Node Void 5 Void))) = False
inTree :: Ord a => a -> BTree a -> Bool
inTree a (Void) = False 
inTree a (Node b d c) = a==d || inTree a b || inTree a c
    


-- | maximo.Define la función maximo que regresa el elemento con mayor valor en un árbol.
--
-- --> maximo (Node (Node Void 2 Void) 1 (Node (Node Void 4 Void) 3 (Node Void 5 Void))) = 5
-- --> maximo (Node (Node Void 8 Void) 3 (Node (Node Void 0 Void) 2 (Node Void 4 Void))) = 8
maximo :: Ord a => BTree a -> a
maximo (Node Void a Void) = a
maximo (Node a b c) = maximum [maximo a,b,maximo c] 

-- | minimo.Define la función minimo que regresa el elemento con menor valor en un árbol.
--
-- --> minimo (Node (Node Void 2 Void) 1 (Node (Node Void 4 Void) 3 (Node Void 5 Void))) = 1
-- --> minimo (Node (Node Void 8 Void) 3 (Node (Node Void 0 Void) 2 (Node Void 4 Void))) = 0
minimo :: Ord a => BTree a -> a
minimo (Node Void x Void) = x
minimo (Node x l r) = minimum [minimo x,l, minimo r]

-- | Define la función firstSnoc que regresa el primer elemento de una lista Snoc.
--
-- --> firstSnoc (Snoc(Snoc(Snoc Empty 1)2)3) = 1
-- --> firstSnoc (Snoc(Snoc(Snoc Empty 3)2)1) = 3
firstSnoc :: SnocList a -> a   
firstSnoc (Snoc Empty a) = a 
firstSnoc (Snoc b a) = firstSnoc b;

-- | Define la función lengthSnoc que regresa la longitud de una lista Snoc.
--
-- --> lengthSnoc (Snoc(Snoc(Snoc Empty 3)2)1) = 3
-- --> lengthSnoc (Snoc(Snoc(Snoc(Snoc(Snoc(Snoc Empty 1)2)3)4)5)6) = 6
lengthSnoc :: SnocList a -> Int
lengthSnoc Empty = 0
lengthSnoc (Snoc a b) = 1 + lengthSnoc a

-- | Define la función concatSnoc que regresa la concatenación de dos listas Snoc.
--
-- --> concatSnoc (Snoc(Snoc(Snoc Empty 1)2)3) (Snoc(Snoc(Snoc Empty 4)5)6) = Snoc (Snoc (Snoc (Snoc (Snoc (Snoc Empty 1) 2) 3) 4) 5) 6
-- --> concatSnoc Empty (Snoc(Snoc Empty 1)2) = Snoc (Snoc Empty 1) 2
concatSnoc :: SnocList a -> SnocList a -> SnocList a
concatSnoc ls (Snoc Empty a) = (Snoc ls a)
concatSnoc ls (Snoc a b)     = Snoc (concatSnoc ls a) b
