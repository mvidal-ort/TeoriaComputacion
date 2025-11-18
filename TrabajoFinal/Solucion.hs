{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Solucion where

-- Parte 1: Verificadores y Reduccion en Haskell

-- 1.1 Representacion de dominios y soluciones

--Representación del problema A (SAT)
-- Fórmula → lista de cláusulas
-- Cláusula → lista de literales
-- Literal → variable positiva o negada
-- Variable → nombre (cadena)

type Literal  = (String, Bool) -- (nombre, positivo/negativo)
type Clause   = [Literal] -- disyunción de literales 
type Formula  = [Clause] -- conjunción de cláusulas 

type DomA = Formula

type SolA = [(String, Bool)]

-- SolA debe contener asignaciones para todas las variables que aparecen en la fórmula.
-- Si falta alguna variable → la evaluación de SAT no está definida correctamente.
-- SolA puede incluir asignaciones extra (variables no usadas), porque se pueden ignorar.
-- Para cumplir la definición estándar de SAT, SolA debe ser una valuación total sobre el conjunto de variables relevantes.

type Point = Int

type DistanceMatrix = [[Int]]
type BaseDistances  = [(Point, Int)]
type Exclusions     = [(Point, Point)]
type Priorities     = [(Point, Int)]

type DomB =
  ( [Point]
  , DistanceMatrix
  , BaseDistances
  , Exclusions
  , Priorities
  , Int  -- maxDistance M
  , Int  -- minPriority V
  )

type SolB = [Point]

-- IMPLEMENTACION VERIFYA ----

lookupVar :: SolA -> String -> Bool --Busca la variable dentro de la solA
lookupVar sol v =
  case lookup v sol of
    Just val -> val
    Nothing  -> error ("Variable no asignada: " ++ v)

evalLiteral :: SolA -> Literal -> Bool --
evalLiteral sol (var, sign) =
  let val = lookupVar sol var
  in if sign then val else not val

evalClause :: SolA -> Clause -> Bool
evalClause sol clause =
  any (evalLiteral sol) clause --any espera una funcion y una lista y devuelve True, si la funcion aplicada a cada elemento devuelve al menos un true

verifyA :: (DomA, SolA) -> Bool
verifyA (formula, sol) =
  all (evalClause sol) formula --all espera una funcion y una lista y devuelve True, si la funcion aplicada a cada elemento es True

-- Es polinomial porque:
-- evalLiteral → constante
-- evalClause → lineal en el tamaño de la cláusula
-- verifyA → lineal en el número de cláusulas
-- Hay que justificar esto en el informe


-- IMPLEMENTACION VERIFYB ----

-- ¿Cómo accedo a D(pi,pj)D(pi,pj) en una [[Int]]?
-- D(i,j)=distanceMatrix!!(i−1)!!(j−1) 
-- !! es el operador de indexación de listas (0-based).
-- Restamos 1 porque los puntos empiezan en 1, pero las listas en Haskell empiezan en 0.

-- Para evitar complicarse con los indices, usar funcion auxiliar que convierta a los identificadores de los puntos
dist :: DistanceMatrix -> Point -> Point -> Int
dist dm p q = dm !! (p - 1) !! (q - 1)

-- Para obtener las distancias desde la matriz de entrada (basedistances)
lookupBaseDist :: BaseDistances -> Point -> Int
lookupBaseDist bd p =
  case lookup p bd of
    Just d  -> d
    Nothing -> error ("Punto no encontrado en baseDistances: " ++ show p)

-- Calculo de la distancia total
totalDistance :: DistanceMatrix -> BaseDistances -> SolB -> Int
totalDistance dm bd route =
  let start = head route
      end   = last route
      baseStart = lookupBaseDist bd start -- distancia base → primer punto
      baseEnd   = lookupBaseDist bd end -- distancia último punto → base      
      middleDist = distBetweenConsecutive dm route -- suma de distancias internas entre pares consecutivos
  in baseStart + middleDist + baseEnd

distBetweenConsecutive :: DistanceMatrix -> [Point] -> Int
distBetweenConsecutive _  []  = 0
distBetweenConsecutive _  [_] = 0   -- una lista con un solo punto no tiene tramos internos
distBetweenConsecutive dm (p:q:rest) =
  dist dm p q + distBetweenConsecutive dm (q:rest)

-- La función r:P→N se representa como una lista (Point, Int) 
-- porque en un lenguaje puramente declarativo como Haskell, las entradas del problema deben ser datos, no funciones.
-- Esta lista es equivalente a una tabla de valores de la función r.
-- Para obtener r(p), el programa simplemente aplica lookup sobre la lista.
-- El usuario (o la instancia del problema) provee la lista completa como parte del DomB.

lookupPriority :: Priorities -> Point -> Int
lookupPriority pr p =
  case lookup p pr of
    Just b  -> b
    Nothing -> error ("Punto no encontrado en priorities: " ++ show p)

totalPriority :: Priorities -> SolB -> Int
totalPriority pr route =
  sum (map (lookupPriority pr) route)

violatesExclusions :: Exclusions -> SolB -> Bool
violatesExclusions excl route =
  any (\(a,b) -> a `elem` route && b `elem` route) excl

verifyB :: (DomB, SolB) -> Bool
verifyB (dom, route) =
  let (_allPoints, dm, bd, excl, pr, maxD, minP) = dom
  in  not (null route)                          -- la ruta no puede estar vacía
      && not (violatesExclusions excl route)    -- no viola exclusiones
      && totalPriority pr route >= minP         -- alcanza prioridad mínima
      && totalDistance dm bd route <= maxD      -- cumple máximo de distancia

-- Todo es polinomial:
-- Exclusiones: O(|E| × |ruta|)
-- Priorización: O(|ruta|)
-- Distancias: O(|ruta|)
-- Lookups simples en listas pequeñas
-- Así que verifyB pertenece a P, como requiere la definición de revisor para problemas NP.

-- LA LETRA PIDE EXPLICITAMENTE EN EL PUNTO 1.2:
-- Justificar formalmente que ambas funciones pueden evaluarse en tiempo polinomial respecto al tamaño
-- de la entrada.

--- 1.3
-- SolveA 

