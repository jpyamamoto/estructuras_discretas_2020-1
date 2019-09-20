-- | Estructuras Discretas 2020-1
-- | Práctica 3: Lógica Proposicional
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes:
-- | Integrante 1: Juan Pablo Yamamoto Zazueta 420002457 jpyamamoto@ciencias.unam.mx
-- | Integrante 2: Valeria Reyes Tapia 317350113 valeria.tapia@ciencias.unam.mx

module Practica3 where

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O
            |P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq)

data Formula = Prop Var
              | Verdadero
              | Falso
              | Neg Formula
              | Formula :&: Formula
              | Formula :|: Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula deriving (Show, Eq)

infixl 9 :&:
infixl 9 :|:
infixr 8 :=>:
infixl 7 :<=>:

type Estado = [Var]

-- | negar. Regresa la fórmula simplificada que resulta de negar la fórmula recibida.
--
-- --> negar (Prop A)       = Neg (Prop A)
-- --> negar Verdadero      = Falso
-- --> negar Falso          = Verdadero
-- --> negar (Neg (Prop A)) = Prop A
negar :: Formula -> Formula
negar (Prop x)    = Neg (Prop x)
negar Verdadero   = Falso
negar Falso       = Verdadero
negar (Neg a)     = a
negar (a :&: b)   = (negar a) :|: (negar b)
negar (a :|: b)   = (negar a) :&: (negar b)
negar (a :=>: b)  = negar (Neg a :|: b)
negar (a :<=>: b) = negar ((a :=>: b) :&: (b :=>: a))

-- | eliminacion. Regresa la fórmula recibida pero sin símbolos de implicación o doble implicación.
--
-- --> eliminacion
eliminacion :: Formula -> Formula
eliminacion (Prop x)    = (Prop x)
eliminacion Verdadero   = Verdadero
eliminacion Falso       = Falso
eliminacion (Neg a)     = negar (eliminacion a)
eliminacion (a :&: b)   = (eliminacion a) :&: (eliminacion b)
eliminacion (a :|: b)   = (eliminacion a) :|: (eliminacion b)
eliminacion (a :=>: b)  = (negar a) :|: b
eliminacion (a :<=>: b) = (eliminacion (a :=>: b)) :&: (eliminacion (b :=>: a))

-- | vars. Regresa una lista con las variables que contiene una fórmula.
--
-- --> vars
vars :: Formula -> [Var]
vars a = eliminaRep (varsAux a)

varsAux :: Formula -> [Var]
varsAux (Prop x)    = [x]
varsAux Verdadero   = []
varsAux Falso       = []
varsAux (Neg a)     = vars a
varsAux (a :&: b)   = vars a ++ vars b
varsAux (a :|: b)   = vars a ++ vars b
varsAux (a :=>: b)  = vars a ++ vars b
varsAux (a :<=>: b) = vars a ++ vars b

-- | interp. Regresa a qué valor evalúa una fórmula dados los estados de sus variables.
--
-- --> interp
interp :: Estado -> Formula -> Bool
interp estados (Prop x)    = if esElemento x estados then True else False
interp estados Verdadero   = True
interp estados Falso       = False
interp estados (Neg a)     = not (interp estados a)
interp estados (a :&: b)   = (interp estados a) && (interp estados b)
interp estados (a :|: b)   = (interp estados a) || (interp estados b)
interp estados (a :=>: b)  = interp estados (eliminacion (a :=>: b))
interp estados (a :<=>: b) = interp estados (eliminacion (a :<=>: b))

-- | conjPotencia. Regresa el conjunto potencia de una lista.
--
-- --> conjPotencia
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = conjPotencia xs ++ conjPotAux x (conjPotencia xs)
  where conjPotAux x xs = [[x] ++ ys | ys <- xs]

-- | estados. Regresa los posibles estados para las variables involucradas en una fórmula.
--
-- --> estados
estados :: Formula -> [Estado]
estados formula = conjPotencia (vars formula)

-- | tautologia. Regresa True si es tautología, False en otro caso.
--
-- --> tautologia
tautologia :: Formula -> Bool
tautologia formula = and' [interp estado formula | estado <- estados formula]

-- | contradiccion. Regresa True si es contradicción, False en otro caso.
--
-- --> contradiccion
contradiccion :: Formula -> Bool
contradiccion formula = and' [interp estado negacion | estado <- estados formula]
  where negacion = negar formula

-- | contingencia. Regresa True si es contingencia, False en otro caso.
--
-- --> contingencia
contingencia :: Formula -> Bool
contingencia formula = not (tautologia formula) && not (contradiccion formula)

-- **************************
-- *                        *
-- *   Métodos Auxiliares   *
-- *                        *
-- **************************

-- | esElemento. Decide si un valor es un elemento de una lista.
--
-- --> esElemento 4 [1,2,3,4,5] = True
-- --> esElemento 9 [1,2,3,4,5] = False
esElemento :: Eq a => a -> [a] -> Bool
esElemento _ [] = False
esElemento y (x:xs) = if y /= x then esElemento y xs else True

-- | eliminaRep. Elimina los elementos repetidos de una lista.
--
-- --> eliminaRep [1,2,3,4,5,6,1,2,3] = [1,2,3,4,5,6]
eliminaRep :: Eq a => [a] -> [a]
eliminaRep xs = auxEliminaRep xs []
  where auxEliminaRep [] _ = []
        auxEliminaRep (x:xs) ys = if esElemento x ys then auxEliminaRep xs ys else x:(auxEliminaRep xs (x:ys))

-- | and'. Determina si todos los valores en la lista son verdaderos.
--
-- --> and' [True, True, True] = True
-- --> and' [True, True, False] = False
and' :: [Bool] -> Bool
and' [] = True
and' (False:xs) = False
and' (x:xs) = and' xs

