{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico0 where

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
data E = Var X | Empty | Unit Z | Pert Z E | Union E E | Intersection E E | Difference E E | Inclusion E E | Assig X E

type X = String
type Z = Int

-- 2)
data V = B Bool | C [Z] deriving Show

-- 3)
-- 3.1)
type M = [(X,V)]

-- 3.2) (lkup: x M7→ v)
lkup :: X -> M -> V
lkup x [] = error "Variable no definida"
lkup x ((x',v):m) 
    | x == x' = v
    | otherwise lkup x m
-- 3.3) (upd: M ≺+(x, v))

-- 4)
