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
type Clause   = [Literal]
type Formula  = [Clause]

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

-- ¿Cómo accedo a D(pi,pj)D(pi,pj) en una [[Int]]?
-- D(i,j)=distanceMatrix!!(i−1)!!(j−1) 
-- !! es el operador de indexación de listas (0-based).
-- Restamos 1 porque los puntos empiezan en 1, pero las listas en Haskell empiezan en 0.

-- Para evitar complicarse con los indices, usar funcion auxiliar que convierta a los identificadores de los puntos
-- dist :: DistanceMatrix -> Point -> Point -> Int
-- dist dm p q = dm !! (p - 1) !! (q - 1)


