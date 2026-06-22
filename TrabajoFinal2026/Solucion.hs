{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Dada una fórmula booleana en forma normal conjuntiva (CNF), decidir si existe una asignación de valores
-- de verdad a sus variables que haga que la fórmula sea verdadera.
-- La Forma Normal Conjuntiva (FNC) conjunción de disyunciones, donde se unen varias cláusulas mediante ANDs
-- y cada cláusula contiene literales unidos por ORs

module Solucion where

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
  , [Edge]            -- Transiciones Permitidas E
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


-- 1.2 Verificadores en tiempo polinomial

-- Extrae las variables de la formula (AUXILIAR)
variablesFormula :: Formula -> [String]
variablesFormula formula = nub [ var | clausula <- formula, (var,_) <- clausula ]
-- De la formula se queda con las clausulas, de cada clausla se queda con la parte de la variable y descarta el bool

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
verifyB ((vs, es, costos, p, x, k), sol) =
       solucionBienFormadaB (vs, es, costos, p, x, k) sol
    && transicionesValidas es sol
    && precedenciasValidas p sol
    && exclusionesValidas x sol
    && costoValido costos k sol

-- una unica aparición de una estacion (AUXILIAR)
-- es la misma que se usa en verifyA
-- sinRepetidos :: Eq a => [a] -> Bool
-- sinRepetidos xs = length xs == length (nub xs)

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
-- Obtengo la posicion de la estación en la lista solución
posicion :: Eq a => a -> [a] -> Int
posicion x xs =
    head [ i | (i,y) <- zip [0..] xs, x == y ]
-- Se genera una lista de pares (posición, estación) usando zip. 
-- Se filtra aquellos pares cuyo vértice coincide con el buscado y me quedo únicamente con la posición. 
-- Como la solución no tiene estaciones repetidas, el resultado es una lista con un único índice
-- Hago head para quedarme con ese elemento (posicion del elemento x en la lista solución que me pasan)

-- Verifica que la posicion de cada estacion en la lista solucion respete las precedencias [Edge]
precedenciasValidas :: [Edge] -> SolB -> Bool
precedenciasValidas ps sol =
    all precedenciaValida ps
  where
    precedenciaValida (u,v) =
        posicion u sol < posicion v sol

-- Costo de una arista obtenida de w : E →R≥0 asigna un costo a cada transicion
costoArista :: [(Edge, Int)] -> Edge -> Int
costoArista costos e =
    case lookup e costos of
        Just c  -> c
        Nothing -> error "Arista sin costo asociado"

-- Costo total del recorrido.
costoRecorrido :: [(Edge, Int)] -> SolB -> Int
costoRecorrido costos sol =
    sum (map (costoArista costos) (aristasRecorridas sol))
-- aristasRecorridas es la funcion que arma las aristas a partir de las estaciones de la solucion 

-- verifica contra la cota k
costoValido :: [(Edge, Int)] -> Int -> SolB -> Bool
costoValido costos k sol =
    costoRecorrido costos sol <= k

-----
-- 1.3 Resolucion en tiempo exponencial
-- solveA:
-- Extraer todas las variables de la fórmula.
-- Generar todas las asignaciones posibles.
-- Buscar la primera que satisfaga verifyA.
-- Devolverla.

-- Extraer las variables de la fórmula (YA ESTA HECHA)
-- variablesFormula :: Formula -> [String]
-- variablesFormula formula = nub [ var | clausula <- formula, (var, _) <- clausula ]

-- Generar todas las posibles soluciones a probar
-- Aca aparece el factor 2^n
generarAsignaciones :: [String] -> [SolA]
generarAsignaciones [] = [[]]
generarAsignaciones (v:vs) = [(v,valor) : asignacion | valor <- [True, False], asignacion <- generarAsignaciones vs]
-- Construir una lista cuyos elementos son (v,valor) : asignacion, donde valor toma sucesivamente los valores de la lista [True, False] 
-- y asignacion toma sucesivamente los valores generados por generarAsignaciones vs.
-- Para cada valor posible de valor y para cada asignación generada para vs, 
-- construir una nueva asignación agregando el par (v,valor) al comienzo de dicha asignación.

-- Genera todas las soluciones que satisfacen la formula o vacia si no 
asignacionesValidas :: Formula -> [SolA]
asignacionesValidas formula = [ asignacion | asignacion <- generarAsignaciones (variablesFormula formula), verifyA (formula, asignacion)]
-- Las prueba todas, no es eficiente, porque podría cortar en la primera que encuentra.

-- hace case en la lista de soluciones generada
solveA :: DomA -> SolA
solveA formula = case asignacionesValidas formula of
                    [] -> error "La formula no es satisfacible"
                    (s:_) -> s
              
-- solveB:
-- genera todas las soluciones válidas, usa permutation de Data.List
-- Aca aparece el orden no polinomial. Permutations es de orden factorial, asi que como mínimo es eso.
solucionesValidasB :: DomB -> [SolB]
solucionesValidasB dom@(vs, _, _, _, _, _) = [sol | sol <- permutations vs, verifyB (dom, sol)]

-- hace case en la lista de soluciones generada
solveB :: DomB -> SolB
solveB dom = case solucionesValidasB dom of
                [] -> error "La instancia no tiene solucion"
                (s:_) -> s

-------
-- =========================
-- PRUEBAS solveA
-- =========================

testSolveA1 :: SolA
testSolveA1 =
    solveA
      [ [("x",True)] ]

testSolveA2 :: SolA
testSolveA2 =
    solveA
      [ [("x",True),("y",True)] ]

formulaA3 :: Formula
formulaA3 =
    [ [("x",True),("y",False)]
    , [("y",True),("z",True)]
    ]

testSolveA3 :: SolA
testSolveA3 = solveA formulaA3

formulaA4 :: Formula
formulaA4 =
    [ [("x",True)]
    , [("x",False)]
    ]

-- Si evalúas testSolveA4 dará error
testSolveA4 :: SolA
testSolveA4 = solveA formulaA4

formulaA5 :: Formula
formulaA5 =
    [ [("x",True),("y",True)]
    , [("x",False),("z",True)]
    ]

testSolveA5 :: Bool
testSolveA5 =
    verifyA (formulaA5, solveA formulaA5)


-- =========================
-- PRUEBAS solveB
-- =========================

domB1 :: DomB
domB1 =
    ( ["A","B","C"]
    , [("A","B"),("B","C"),("C","A")]
    , [(("A","B"),10),(("B","C"),5),(("C","A"),8)]
    , []
    , []
    , 30
    )

testSolveB1 :: SolB
testSolveB1 = solveB domB1

domB2 :: DomB
domB2 =
    ( ["A","B","C"]
    , [("A","B"),("B","C"),("C","A")]
    , [(("A","B"),10),(("B","C"),5),(("C","A"),8)]
    , []
    , []
    , 23
    )

testSolveB2 :: SolB
testSolveB2 = solveB domB2

domB3 :: DomB
domB3 =
    ( ["A","B","C"]
    , [("A","B"),("B","C"),("C","A")]
    , [(("A","B"),10),(("B","C"),5),(("C","A"),8)]
    , []
    , []
    , 20
    )

-- Da error
testSolveB3 :: SolB
testSolveB3 = solveB domB3

domB4 :: DomB
domB4 =
    ( ["A","B","C"]
    , [("A","B"),("B","C"),("C","A")]
    , [(("A","B"),10),(("B","C"),5),(("C","A"),8)]
    , [("A","C")]
    , []
    , 30
    )

testSolveB4 :: SolB
testSolveB4 = solveB domB4

domB5 :: DomB
domB5 =
    ( ["A","B"]
    , [("A","B"),("B","A")]
    , [(("A","B"),5),(("B","A"),5)]
    , [("A","B"),("B","A")]
    , []
    , 20
    )

-- Da error
testSolveB5 :: SolB
testSolveB5 = solveB domB5

domB6 :: DomB
domB6 =
    ( ["A","B","C"]
    , [("A","B"),("B","C"),("C","A")]
    , [(("A","B"),10),(("B","C"),5),(("C","A"),8)]
    , []
    , [("A","B")]
    , 30
    )

-- Da error
testSolveB6 :: SolB
testSolveB6 = solveB domB6

domB7 :: DomB
domB7 =
    ( ["A","B","C"]
    , [("A","B"),("B","C")]
    , [(("A","B"),10),(("B","C"),5)]
    , []
    , []
    , 30
    )

-- Da error
testSolveB7 :: SolB
testSolveB7 = solveB domB7

domB8 :: DomB
domB8 =
    ( ["A","B","C","D"]
    , [("A","B"),("B","C"),("C","D"),("D","A")]
    , [(("A","B"),1),(("B","C"),1),(("C","D"),1),(("D","A"),1)]
    , []
    , []
    , 10
    )

testSolveB8 :: SolB
testSolveB8 = solveB domB8

domB9 :: DomB
domB9 =
    ( ["A","B","C","D"]
    , [("A","B"),("B","C"),("C","D"),("D","A")]
    , [(("A","B"),2),(("B","C"),2),(("C","D"),2),(("D","A"),2)]
    , []
    , []
    , 8
    )

testSolveB9 :: SolB
testSolveB9 = solveB domB9

testSolveB10 :: Bool
testSolveB10 =
    verifyB (domB1, solveB domB1)