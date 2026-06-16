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

-- Controlar que la solución esté bien formada:
-- Toda variable que aparece en la fórmula debe aparecer en la solución.
-- No hay variables repetidas
-- No hay variables adicionales

-- Extrae las variables de la formula (AUXILIAR)
variablesFormula :: Formula -> [String]
variablesFormula formula =
    nub [ var | clausula <- formula
              , (var,_) <- clausula ]

-- Extrae las variables de la solución (AUXILIAR)
variablesSolucion :: SolA -> [String]
variablesSolucion sol = map fst sol

-- Verifica que no haya repetidos
sinRepetidos :: Eq a => [a] -> Bool
sinRepetidos xs = length xs == length (nub xs)
    
-- Verifica que no faltan variables
noFaltanVariables :: Formula -> SolA -> Bool
noFaltanVariables formula sol =
    all (`elem` varsSol) varsFormula
  where
    varsFormula = variablesFormula formula
    varsSol = variablesSolucion sol

-- Verifica que no hay varibles adicionales
sinVariablesExtra :: Formula -> SolA -> Bool
sinVariablesExtra formula sol =
    all (`elem` varsFormula) varsSol
  where
    varsFormula = variablesFormula formula
    varsSol = variablesSolucion sol

-- Solución bien formada
solucionBienFormada :: Formula -> SolA -> Bool
solucionBienFormada formula sol =
       sinRepetidos (variablesSolucion sol)
    && noFaltanVariables formula sol
    && sinVariablesExtra formula sol

-- Recibe una formula a satisfacer y una solución a evaluar, si todas las clausulas de la solucion son true, entonces la solución es true
verifyA :: (DomA, SolA) -> Bool
verifyA (formula, sol) = solucionBienFormada formula sol && all (evalClause sol) formula
-- all espera una funcion y una lista y devuelve True, si la funcion aplicada a cada elemento es True
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

-----
verifyB :: (DomB, SolB) -> Bool
verifyB (dom, sol) =
       solucionBienFormadaB dom sol
    && coberturaTotal dom sol
    && transicionesValidas dom sol
    && precedenciasValidas dom sol
    && exclusionesValidas dom sol
    && costoValido dom sol

-- una unica aparición de una estacion (AUXILIAR)
sinRepetidos :: Eq a => [a] -> Bool
sinRepetidos xs = length xs == length (nub xs)

-- Toda estación de debe aparecer en la solución
noFaltanVertices :: [Vertex] -> SolB -> Bool
noFaltanVertices vs sol = all (`elem` sol) vs

-- No hay estaciones adicionales
sinVerticesExtra :: [Vertex] -> SolB -> Bool
sinVerticesExtra vs sol = all (`elem` vs) sol

-- Solución bien formada
solucionBienFormadaB :: DomB -> SolB -> Bool
solucionBienFormadaB (vs,_,_,_,_,_) sol =
       sinRepetidos sol
    && noFaltanVertices vs sol
    && sinVerticesExtra vs sol

-- Funcion auxiliar para armar las aristas del grafo a recorrer (AUXILIAR)
aristasRecorridas :: SolB -> [Edge]
aristasRecorridas [] = []
aristasRecorridas vs = zip vs (tail vs ++ [head vs]) -- supone que la lista tendra mas de una estacion
-- Sirve para verificar las precedencias, las exclusiones y la cota de costo

-- Transiciones válidas. A partir de las aristas de la solución, verifica que todas estén en las transiciones válidas [Edge]
transicionesValidas :: [Edge] -> SolB -> Bool
transicionesValidas es sol = all (`elem` es) (aristasRecorridas sol)

-- Exlusiones. A partir de las aristas de la solucion, verifica que ninguna esté en la lista de exlusiones [Edge]
exclusionesValidas :: [Edge] -> SolB -> Bool
exclusionesValidas xs sol = all (`notElem` aristasRecorridas sol) xs

--- Precedencia
-- Obtengo la posicon de la estación en la lista solución
posicion :: Eq a => a -> [a] -> Int
posicion x xs =
    head [ i | (i,y) <- zip [0..] xs, x == y ]

-- Verifica que la posicion de cada estacion en la lista solucion respete las precedencias de [Edge]
precedenciasValidas :: [Edge] -> SolB -> Bool
precedenciasValidas ps sol =
    all precedenciaValida ps
  where
    precedenciaValida (u,v) =
        posicion u sol < posicion v sol