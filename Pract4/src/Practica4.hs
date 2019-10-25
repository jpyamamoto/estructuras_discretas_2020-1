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

igualNat :: Nat -> Nat -> Bool
igualNat Zero Zero       = True
igualNat (Suc m) (Suc n) = igualNat m n
igualNat _ _             = False

natToInt :: Nat -> Int
natToInt Zero   = 0
natToInt (Suc n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = Suc (intToNat (x-1))

restaNat :: Nat -> Nat -> Nat
restaNat numero Zero     = numero
restaNat (Suc n) (Suc m) = restaNat n m

preorden :: BTree a -> [a]
preorden Void         = []
preorden (Node i r d) = [r] ++ (preorden i) ++ (preorden d)

inorden :: BTree a -> [a]
inorden Void         = []
inorden (Node i r d) = (inorden i) ++ [r] ++ (inorden d)

postorden :: BTree a -> [a]
postorden Void         = []
postorden (Node i r d) = (postorden i) ++ (postorden d) ++ [r]

agregaOrd :: Ord a => a -> BTree a -> BTree a
agregaOrd x (Node i r d) = if r > x && (todosMenores x i)
                              then (Node (Node i x Void) r d)
                              else (Node (agregaOrd x i) r d)

