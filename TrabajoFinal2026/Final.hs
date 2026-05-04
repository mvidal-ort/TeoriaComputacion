{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Dada una fórmula booleana en forma normal conjuntiva (CNF), decidir si existe una asignación de valores
-- de verdad a sus variables que haga que la fórmula sea verdadera.
-- La Forma Normal Conjuntiva (FNC) conjunción de disyunciones, donde se unen varias cláusulas mediante ANDs
-- y cada cláusula contiene literales unidos por ORs

module Final where

import Data.List

--Ejercicio 1: Verificadores y solucionadores en Haskell
--1.1 Representación de dominios y soluciones

type Literal  = (String, Bool) -- (nombre de la variable, signo)
type Clause   = [Literal]      -- Lista de literales, que representan los ORs
type Formula  = [Clause]       -- Lista de clausulas que representan los ANDs

type DomA = Formula            -- Este es el problema que queremos resolver
type SolA = [(String, Bool)]   -- Cada solucion es una lista de variables y su signo, que se prueban en la formula

-- PREGUNTAR:
-- SolA debe contener asignaciones para todas las variables que aparecen en la fórmula.
-- Si falta alguna variable → la evaluación de SAT no está definida correctamente.
-- SolA puede incluir asignaciones extra (variables no usadas), porque se pueden ignorar.
-- Para cumplir la definición estándar de SAT, SolA debe ser una valuación total sobre el conjunto de variables relevantes.

-- Recibe una formula a satisfacer y una solución a evaluar, si todas las clausulas de la solucion son true, entonces la solución es true
verifyA :: (DomA, SolA) -> Bool
verifyA (formula, sol) =
  all (evalClause sol) formula --all espera una funcion y una lista y devuelve True, si la funcion aplicada a cada elemento es True
  -- Se currifica para usar (evalClause sol) como la funcion que espera all

-- Evalua una clausula de SolA. Como cada clausula es la union de ORs, si algun literal de cada clausula es true, entonces es true
evalClause :: SolA -> Clause -> Bool
evalClause sol clause =
  any (evalLiteral sol) clause --any espera una funcion y una lista y devuelve True, si la funcion aplicada a cada elemento devuelve al menos un true
  -- Se currifica para usar (evalLiteral sol) como la funcion que espera any

evalLiteral :: SolA -> Literal -> Bool 
evalLiteral sol (var, sign) =
  let val = lookupVar sol var
  in if sign then val else not val

lookupVar :: SolA -> String -> Bool --Busca la variable dentro de la solA
lookupVar sol v =
  case lookup v sol of
    Just val -> val
    Nothing  -> error ("Variable no asignada: " ++ v)



--type DomB = undefined
--type SolB = undefined
