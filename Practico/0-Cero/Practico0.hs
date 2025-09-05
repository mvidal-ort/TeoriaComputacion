{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico0 where

import Data.List (subsequences)

-- eval  m (Pert z e) = let (m', C c) = eval m e    
--                         in (m', belongs z c)

-- Con Let le estoy diciendo que el resultado de eval m e, lo tire en el par (m', C c)
-- A su vez, con C c, le estoy diciendo que ese resultado es de tipo Conjunto.
-- let crea el ambiente el in permite invocar las variables en ese ambiente
-- Eventualmente se podria no poner el constructor y dejar solo (m',c) y hacer un case
-- para que la lista de enteros sea un conjunto. Es mas facil asi.
-- El problema es que asi no compila, porque el belons z c tiene que devolver un valor de tipo Bool
-- por eso hay que ponerle el constructor:
-- eval  m (Pert z e) = let (m', C c) = eval m e    
--                         in (m', B (belongs z c))


-- La recomendación es hacer un let por cada condicion que aparezca en la regla que se esta evaluando
-- Asi te podes guardar todos los nombres y usarlos en los in.

-- eval m (Union e1 e2) = let (m', C c1) = eval m e1
--                         in let (m'', C c2) = eval m' e2
--                           in (m'', C (union c1 c2))


-- e1, e2 ::= x (variable)
-- | ∅ (conjunto vac´ıo)
-- | {z} (conjunto unitario)
-- | z ∈ e1 (pertenencia)
-- | e1 ∪ e2 (uni´on)
-- | e1 ∩ e2 (intersecci´on)
-- | e1 − e2 (diferencia)
-- | e1 ⊆ e2 (inclusi´on)
-- | x := e1 (asignaci´on)
-- x ::= String (identificador de las variables)
-- z ::= Z (enteros)

-- 1)
data E = Var X | Empty | Unit Z | Pert Z E | Union E E | Intersection E E | Difference E E | Inclusion E E | Assig X E | Pot E | Equal E E | Len E

-- El data E = Var X | Empty | Unit Z | Pert Z E | Union E E | Intersection E E | Difference E E | Inclusion E E | Assig X E
-- Se extendio por la parte 6

-- 6. Realizar todos los cambios necesarios para que el lenguaje desarrollado en
-- las partes anteriores permita hallar:
-- (a) El conjunto potencia (o partes) de un conjunto dado
-- (b) Dados dos conjuntos determinar si son iguales
-- (c) Calcular el largo/cardinal de un conjunto dado


type X = String
type Z = Int

-- 2)
data V = B Bool | C [Z] | P[[Z]] | I Int deriving Show

-- Por 6) extiende tambien el data V = B Bool | C [Z] 
-- ya que se necesita conjunto de conjuntos para potencia y se necesita enteros para cardinal de un conjunto

-- 3)
-- 3.1)
type M = [(X,V)]

-- 3.2) (lkup: x M7→ v)
lkup :: X -> M -> V
lkup x [] = error "Variable no definida"
lkup x ((x',v):m) 
    | x == x' = v
    | otherwise =  lkup x m

-- 3.3) (upd: M ≺+(x, v))
upd :: M -> (X,V) -> M
upd [] (x0,v0) = [(x0,v0)]
upd ((x,v):xs) (x0,v0) 
    | (x == x0) = (x0,v0):xs
    | otherwise = (x,v):(upd xs (x0,v0))

-- 4) Reglas de evaluacion
-- 4.1) Funciones auxiliares

-- • belongs :: Int -> [Int] -> Bool que dado un entero z y un conjunto c, retorna True si z se encuentra en c.
belongs :: Int -> [Int] -> Bool
belongs z [] = False
belongs z (x:xs)
    | (z == x) = True
    | otherwise = belongs z xs

-- • union :: [Int] -> [Int] -> [Int]
union :: [Int] -> [Int] -> [Int]
union [] xs = xs
union xs [] = xs
union (x:xs) ys 
    | (belongs x ys == True) = union xs ys
    | otherwise = union xs (x:ys)
-- Sacar los repetidos

-- • intersection :: [Int] -> [Int] -> [Int] 
intersection :: [Int] -> [Int] -> [Int]
intersection xs [] = []
intersection [] _ = []
intersection (x:xs) (ys)
    | (belongs x ys == True) = x:(intersection xs ys)
    | otherwise = intersection (xs) (ys)
-- Esta version mantiene duplicados de la primera lista, pero asumimos que la lista que se le pasa, ya es un conjunto y no tiene duplicados.
-- Se puede arreglar usando nub:
-- intersection :: [Int] -> [Int] -> [Int]
-- intersection xs ys = nub [x | x <- xs, x `elem` ys]

-- • difference :: [Int] -> [Int] -> [Int]
difference :: [Int] -> [Int] -> [Int]
difference xs [] = xs
difference [] xs = []
difference (x:xs) ys 
    | (belongs x ys == True) = difference xs ys
    | otherwise = x:(difference xs ys)
-- Esta version mantiene duplicados de la primera lista, pero asumimos que la lista que se le pasa, ya es un conjunto y no tiene duplicados.
-- Se puede arreglar usando nub:
-- difference :: [Int] -> [Int] -> [Int]
-- difference xs ys = nub [x | x <- xs, not (x `elem` ys)]

-- • included :: [Int] -> [Int] -> Bool
included :: [Int] -> [Int] -> Bool
included xs [] = False
included [] _ = True
included (x:xs) ys 
    | (belongs x ys == True) = included xs ys
    | otherwise = False

-- 4.2) Definir la funcion (parcial2) de evaluacion o funcion semantica, que recibe una memoria y una expresion, y retorna la memoria actualizada
-- data E = Var X | Empty | Unit Z | Pert Z E | Union E E | Intersection E E | Difference E E | Inclusion E E | Assig X E
eval :: M -> E -> (M,V)

eval m (Var x)  = (m,lkup x m)

eval m Empty = (m, C []) -- hay que forzar el tipo conjunto porque sino interpreta una lista.

eval m (Unit z) = (m, C [z])

eval m (Pert z e) = 
    let (m', v) = eval m e
    in  case v of
        C xs -> (m', B (belongs z xs))
        _ -> error "e no evalua a un conjunto, Pert no tiene sentido"

eval m (Union e1 e2) =
  let (m', C c1) = eval m e1
      (m'', C c2) = eval m' e2
  in (m'', C (union c1 c2))

eval m (Intersection e1 e2) =
    let (m', C c1) = eval m e1
        (m'', C c2) = eval m' e2
    in (m'', C (intersection c1 c2))

eval m (Difference e1 e2) =
    let (m', C c1) = eval m e1
        (m'', C c2) = eval m' e2
    in (m'', C (difference c1 c2))

eval m (Inclusion e1 e2) =
    let (m', C c1) = eval m e1
        (m'', C c2) = eval m' e2
    in (m'', B (included c1 c2))

eval m (Assig x e) =  -- En este caso v puede ser tanto conjuntos como booleanos, por eso no necesita el constructor
    let (m', v) = eval m e
    in (upd m' (x,v), v)
-- Si reemplazo v por C v, compila tambien y solo guarda cuando evalua a Conjuntos y no booleanos
-- Pero puede dar error en tiempo de ejecucion de pattern matching si la expresion evalua a un bool

-- Extendiendo por 6)
eval m (Pot e) =
    let (m', C xs) = eval m e
    in (m', P (subsequences xs))

-- 5) data E = Var X | Empty | Unit Z | Pert Z E | Union E E | Intersection E E | Difference E E | Inclusion E E | Assig X E

-- conj1 ::E, que represente al conjunto {1,2,3}.
conj1 :: E
conj1 = Union ( Union (Unit 1) (Unit 2) ) (Unit 3) 

-- conj2 ::E, que represente al conjunto {2,3,4}.
conj2 :: E
conj2 = Union ( Union (Unit 2) (Unit 3) ) (Unit 4) 

-- conj3 ::E, que represente a la union de conj1 y conj2
-- ({1,2,3} ∪ {2,3,4}).
conj3 :: E
conj3 = Union conj1 conj2

-- conj4 ::E, que represente a la intersecci´on de conj1 y conj2
-- ({1,2,3} ∩ {2,3,4}).
conj4 :: E
conj4 = Intersection conj1 conj2

-- pert1 ::E, que represente a la expresi´on que dice si el entero 2
-- pertenece a conj1 (2 ∈ {1,2,3}).
pert1 ::E
pert1 = Pert 2 conj1

-- pert2 ::E, que represente a la expresi´on que dice si el entero 3
-- pertenece a conj4 (3 ∈ ({1,2,3} ∩ {2,3,4})).
pert2 ::E
pert2 = Pert 2 conj4

-- incl1 ::E, que represente a la expresi´on que dice si conj1 est´a incluido
-- en conj2 ({1,2,3} ⊆ {2,3,4}).
incl1 :: E
incl1 = Inclusion conj1 conj2

-- incl2 ::E, que represente a la expresi´on que dice si conj4 est´a incluido
-- en conj2 (({1,2,3} ∩ {2,3,4}) ⊆ {2,3,4}).
incl2 :: E
incl2 = Inclusion conj4 conj2

-- incl3 ::E, que represente a la expresi´on que dice si conj1 est´a incluido
-- en conj3 ({1,2,3} ⊆ ({1,2,3} ∪ {2,3,4})).
incl3 :: E
incl3 = Inclusion conj1 conj3

-- ass1 ::E, que represente a la expresi´on donde se asigna a una variable
-- w el conjunto conj1 (w := {1,2,3}).
ass1 :: E
ass1 = Assig "w" conj1 

-- ass2 ::E, que represente a la expresi´on donde se asigna a una variable
-- x el conjunto conj4 (x := {1,2,3} ∩ {2,3,4}).
ass2 :: E
ass2 = Assig "x" conj4

-- ass3 ::E, que represente a la expresi´on donde se asigna a una variable
-- y el resultado de pert2 (y := 3 ∈ ({1,2,3} ∩ {2,3,4})).
ass3 :: E
ass3 = Assig "y" pert2

-- ass4 ::E, que represente a la expresi´on donde se asigna a una variable
-- z el resultado de incl2 (z := ({1,2,3} ∩ {2,3,4}) ⊆ {2,3,4}).
ass4 :: E
ass4 = Assig "z" incl2

-- 6) Extendiendo
-- por ejemplo en la parte c) que quiere calcular el largo de un conjunto:
-- hay que extender el data E = Var X | Empty ... Assig X E | Len E
-- hay que extender los valores que puede tomar V = ... | Z Int
-- ya que al hacer eval m (Len e) vamos a querer que el valor sea de tipo Int
-- eval m (Len e) = let (m', C v) = eval m e
--                  in  (m', length v)

-- Para el conjunto potencia (es un conjunto de conjuntos) voy a tener que usar una funcion del preludio que 
-- aparentemente se llama subsequences (investigar)

-- 6. Realizar todos los cambios necesarios para que el lenguaje desarrollado en
-- las partes anteriores permita hallar:

-- data E = Var X | Empty | Unit Z | Pert Z E | Union E E | Intersection E E | Difference E E | Inclusion E E | Assig X E | Pot E | Equal E E | Len E
-- type X = String
-- type Z = Int
-- data V = B Bool | C [Z] | P[[Z]] | I Int deriving Show
-- type M = [(X,V)]

-- (a) El conjunto potencia (o partes) de un conjunto dado

potencia :: [Z] -> [[Z]]
potencia = subsequences
-- la funcion del preludio tiene el mismo tipo por lo cual no es neceario distinguir casos
-- no sé si es necearia la funcion, ya que se puede hacer directamente el eval de Pot y llamar a susequences ahi



-- (b) Dados dos conjuntos determinar si son iguales


-- (c) Calcular el largo/cardinal de un conjunto dado


