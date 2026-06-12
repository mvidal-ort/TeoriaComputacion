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

-- Para SAT
type Literal  = (String, Bool) -- (nombre de la variable, signo)
type Clause   = [Literal]      -- Lista de literales, que representan los ORs
type Formula  = [Clause]       -- Lista de clausulas que representan los ANDs

type DomA = Formula            -- Este es el problema que queremos resolver
type SolA = [(String, Bool)]   -- Cada solucion es una lista de variables y su signo, que se prueban en la formula


-- Para problema B
type Vertex = String
type Edge = (Vertex, Vertex)

type DomB =
  ( [Vertex]          -- Estaciones V
  , [Edge]            -- Tranciciones Permitidas E
  , [(Edge, Int)]     -- Costos de cada transición w
  , [Edge]            -- Precedencias P
  , [Edge]            -- Exclusiones Locales X
  , Int               -- Costo Máximo k
  )
type SolB = [Vertex]

-- ARGUMENTO PARA DOCUMENTAR
-- De hecho, si empezás a agregar chequeos como:
-- verificar que no haya variables repetidas en SolA;
-- verificar que todas las variables de la fórmula aparezcan en SolA;
-- verificar que no haya variables extra;
-- vas a complicar el código sin aportar demasiado al objetivo principal del ejercicio.
-- Además, cuando llegue la justificación de complejidad, es más fácil argumentar sobre el verificador actual.
-- Lo único que sí anotaría para la defensa (aunque no lo programes) es algo como:
-- Se asume que SolA representa una asignación completa de las variables de la fórmula. 
--Si una variable no estuviera asignada, la función lookupVar produciría un error. Esta situación no se contempla porque, por definición, 
--el certificado de SAT es una asignación total sobre las variables de la instancia.
-- Esa observación demuestra que pensaste el tema y evita que el docente te sorprenda con una pregunta sobre casos borde.

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
