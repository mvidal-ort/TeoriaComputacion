{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Solucion where

import Data.List
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

-- Dado un DomA (la fórmula en CNF), solveA debe:
-- Obtener todas las variables de la fórmula.
-- Generar todas las posibles asignaciones (valuaciones) para esas variables.
-- Si hay n variables → hay 2^n valuaciones.
-- Para cada valuación (cada SolA posible), ejecutar verifyA.
-- Retornar True si alguna valuación satisface la fórmula.
-- Retornar False si ninguna lo hace.
-- Esto implementa exactamente la definición de NP:
-- → adivinar (generar) una solución y → verificarla con verifyA.

varsInFormula :: Formula -> [String]
varsInFormula formula =
  removeDuplicates [ v | clause <- formula -- 
                       , (v, _) <- clause ]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

allValuations :: [String] -> [SolA]
allValuations [] = [[]]
allValuations (v:vs) =
  [ (v, False) : sol | sol <- allValuations vs ] ++
  [ (v, True)  : sol | sol <- allValuations vs ]
-- Toma la primera variable v.
-- Genera todas las soluciones donde v = False.
-- Genera todas donde v = True.
-- Para cada caso, pega el valor al inicio.
-- Esto genera exactamente 2^n valuaciones.

-- Prgunta para el chat, donde quiero que muestr las soluciones generadas
printAllValuations :: [String] -> IO ()
printAllValuations vars = do
    let sols = allValuations vars
    mapM_ print sols


solveA :: DomA -> Bool
solveA formula =
  let vars = varsInFormula formula
      sols = allValuations vars
   in any (\sol -> verifyA (formula, sol)) sols
-- varsInFormula obtiene la lista de variables del SAT.
-- allValuations vars genera todas las posibles asignaciones.
-- any prueba cada SolA llamando a verifyA.
-- Si alguna valuación satisface la fórmula → retorna True.
-- Este algoritmo es exponencial, como corresponde a un solver (no a un verificador).

-- Pregunta para el chat en el informe, version que muestra las valuaciones por pantalla
-- solveA :: DomA -> IO Bool
-- solveA formula = do
--   let vars = varsInFormula formula
--   let sols = allValuations vars
--   putStrLn "Generando valuaciones..."
--   checkSols sols
--   where
--     checkSols [] = return False
--     checkSols (s:ss) = do
--         print s                -- ← muestra la valuación por pantalla
--         if verifyA (formula, s)
--            then return True
--            else checkSols ss

-- 1.3)
-- solveB
-- Esta versión devuelve la primera ruta válida encontrada como Maybe SolB (Just ruta si existe, Nothing si no). 
-- Usa combinaciones + permutaciones para generar todas las rutas 
-- (todas las permutaciones de todos los subconjuntos, por tamaños crecientes para encontrar rutas «pequeñas» primero).

-- combinations k xs  => todas las combinaciones (subconjuntos) de tamaño k
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs)
  | k < 0     = []
  | otherwise = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- allRoutes: para una lista de puntos, genera todas las rutas posibles
-- (todas las permutaciones de todos los subconjuntos de tamaño 1..n)
allRoutes :: [Point] -> [[Point]]
allRoutes pts =
  concat [ concatMap permutations (combinations k pts) | k <- [1..length pts] ]

-- -------------------------
-- solveB: búsqueda exhaustiva
-- -------------------------
-- Devuelve la primera ruta válida (Maybe SolB)
solveB :: DomB -> Maybe SolB
solveB dom@(pts, _, _, _, _, _, _) = findFirst (allRoutes pts)
  where
    findFirst :: [[Point]] -> Maybe SolB
    findFirst []     = Nothing
    findFirst (r:rs) =
      if verifyB (dom, r)
        then Just r
        else findFirst rs

-- Variantes útiles (si querés)
-- Detener al primer hallazgo: ya implementado (devuelve la primera que verifique).
-- Imprimir progreso: convertir solveB a IO (Maybe SolB) e imprimir cada N rutas o al encontrar una válida.
-- Poda por distancia/prioridad parcial: construir rutas incrementalmente y podar ramas que ya exceden M o no alcanzan posible V. Esto reduce drásticamente el espacio buscado.
-- Generación incremental: en vez de allRoutes, generar rutas recursivamente y verificar parciales para poda temprana — más eficiente y recomendable si quieres probar instancias medianas.

---------------------------
-- Para el fundamento teorico, si vamos a utilizar un problema intermedio para hacer reducciones, 
-- hay que probar que la transitividad de reduccion pi < pi1 < pi2 => pi > pi2. En particular, las funciones
-- polinomiales que se usar para pasar de uno a otro, pueden componerse en una f(g(y)), que a su vez es polinomial
-- por lo tanto puede servir para hacer la reduccion pi pi2. 26/11 al principio de la clase, lo muestran en el pizarron
-- es un problema de examen.

----------------------------
-- Para Probar

-- Formula1 (x1∨¬x2∨x1)∧(¬x3∨x2)
formula1 :: Formula
formula1 =
  [ [("x1",True), ("x2",False), ("x1",True)]
  , [("x3",False), ("x2",True)]
  ]

formula2 :: Formula
formula2 =
  [ [("x1",True),("x2",False)]   -- x1 ∨ ¬x2
  , [("x1",False),("x2",True)]   -- ¬x1 ∨ x2
  ]

formula3 :: Formula
formula3 =
  [ [("x1",True)]
  , [("x1",False)]
  ]

formulaBig :: Formula
formulaBig =
  [ [("x1",True),  ("x1",False)]
  , [("x2",True),  ("x2",False)]
  , [("x3",True),  ("x3",False)]
  , [("x4",True),  ("x4",False)]
  , [("x5",True),  ("x5",False)]
  , [("x6",True),  ("x6",False)]
  , [("x7",True),  ("x7",False)]
  , [("x8",True),  ("x8",False)]
  , [("x9",True),  ("x9",False)]
  , [("x10",True), ("x10",False)]
  , [("x11",True), ("x11",False)]
  , [("x12",True), ("x12",False)]
  , [("x13",True), ("x13",False)]
  , [("x14",True), ("x14",False)]
  , [("x15",True), ("x15",False)]
  , [("x16",True), ("x16",False)]
  ]

solBig :: SolA
solBig =
  [ ("x1", True)
  , ("x2", False)
  , ("x3", True)
  , ("x4", False)
  , ("x5", True)
  , ("x6", False)
  , ("x7", True)
  , ("x8", False)
  , ("x9", True)
  , ("x10", False)
  , ("x11", True)
  , ("x12", False)
  , ("x13", True)
  , ("x14", False)
  , ("x15", True)
  , ("x16", False)
  ]

formula20 :: Formula
formula20 =
  [ [("x1",True),  ("x1",False)]
  , [("x2",True),  ("x2",False)]
  , [("x3",True),  ("x3",False)]
  , [("x4",True),  ("x4",False)]
  , [("x5",True),  ("x5",False)]
  , [("x6",True),  ("x6",False)]
  , [("x7",True),  ("x7",False)]
  , [("x8",True),  ("x8",False)]
  , [("x9",True),  ("x9",False)]
  , [("x10",True), ("x10",False)]
  , [("x11",True), ("x11",False)]
  , [("x12",True), ("x12",False)]
  , [("x13",True), ("x13",False)]
  , [("x14",True), ("x14",False)]
  , [("x15",True), ("x15",False)]
  , [("x16",True), ("x16",False)]
  , [("x17",True), ("x17",False)]
  , [("x18",True), ("x18",False)]
  , [("x19",True), ("x19",False)]
  , [("x20",True), ("x20",False)]
  ]

sol20 :: SolA
sol20 =
  [ ("x1", True)
  , ("x2", False)
  , ("x3", True)
  , ("x4", False)
  , ("x5", True)
  , ("x6", False)
  , ("x7", True)
  , ("x8", False)
  , ("x9", True)
  , ("x10", False)
  , ("x11", True)
  , ("x12", False)
  , ("x13", True)
  , ("x14", False)
  , ("x15", True)
  , ("x16", False)
  , ("x17", True)
  , ("x18", False)
  , ("x19", True)
  , ("x20", False)
  ]


-- Mencionar en el informe que para evidenciar la diferencia en los ordenes entre solvA y verifyA
-- hay que lograr que explore que explore la mayor cantidad de soluciones posibles. Preferentemente
-- todas al utilizar una formula insatisfacible. Si se satisface con poca exploración, no se nota
-- la diferencia. 

formula20_unsat :: Formula
formula20_unsat =
  [ [("x1",True)] , [("x1",False)]
  , [("x2",True)] , [("x2",False)]
  , [("x3",True)] , [("x3",False)]
  , [("x4",True)] , [("x4",False)]
  , [("x5",True)] , [("x5",False)]
  , [("x6",True)] , [("x6",False)]
  , [("x7",True)] , [("x7",False)]
  , [("x8",True)] , [("x8",False)]
  , [("x9",True)] , [("x9",False)]
  , [("x10",True)], [("x10",False)]
  , [("x11",True)], [("x11",False)]
  , [("x12",True)], [("x12",False)]
  , [("x13",True)], [("x13",False)]
  , [("x14",True)], [("x14",False)]
  , [("x15",True)], [("x15",False)]
  , [("x16",True)], [("x16",False)]
  , [("x17",True)], [("x17",False)]
  , [("x18",True)], [("x18",False)]
  , [("x19",True)], [("x19",False)]
  , [("x20",True)], [("x20",False)]
  ]

-----------------
--PRUEBAS para B

dm :: DistanceMatrix
dm =
  [ [0,4,7]
  , [4,0,3]
  , [7,3,0]
  ]
--dist dm 1 2   -- 4
--dist dm 2 3   -- 3
--dist dm 3 1   -- 7

bd:: BaseDistances
bd = [(1,5),(2,3),(3,6)]
-- lookupBaseDist bd 2  -- 3

pr::Priorities
pr = [(1,1),(2,2),(3,1)]
-- lookupPriority  pr 2 -- 2

excl :: Exclusions
excl = [(1,3)]
-- violatesExclusions excl [2,3]     -- False
-- violatesExclusions excl [1,3]     -- True
-- violatesExclusions excl [3,1]     -- True
-- violatesExclusions excl [1,2,3]   -- True

route :: [Point]
route = [2,3]
-- totalDistance :: DistanceMatrix -> BaseDistances -> SolB -> Int
-- totalDistance dm bd [2,3]   -- 12

domPrueba :: DomB
domPrueba =
 ( [1,2,3]
 , dm
 , bd
 , [(1,3)]
 , [(1,1),(2,2),(3,1)]
 , 20
 , 3
 )
-- verifyB (domPrueba, [2,3])   -- True
-- verifyB (domPrueba, [1,3])   -- False por exclusion
-- verifyB (domPrueba, [1,2,3]) -- False por distancia
-- verifyB (domPrueba, [3]) -- False por prioridad

domB1 :: DomB
domB1 =
  ( [1,2,3]                        -- puntos
  , [ [0,2,5]                      -- matriz de distancias D(i,j)
    , [2,0,3]
    , [5,3,0]
    ]
  , [(1,4),(2,2),(3,6)]            -- distancia a la base r(i)
  , [(1,3)]                        -- exclusión: no se puede visitar 1 y 3 juntos
  , [(1,2),(2,2),(3,1)]            -- prioridades π(i)
  , 15                             -- M: distancia máxima
  , 3                              -- V: prioridad mínima requerida
  )

domB2 :: DomB
domB2 =
  ( [1,2,3] 
  , [ [0,4,4]
    , [4,0,4]
    , [4,4,0]
    ]
  , [(1,10),(2,10),(3,10)]   -- distancia a la base MUY alta
  , []                       -- sin exclusiones
  , [(1,1),(2,1),(3,1)]      -- poca prioridad
  , 10                       -- M = 10 (muy chico)
  , 3                        -- prioridad mínima requerida
  )

domB3 :: DomB
domB3 =
  ( [1,2,3,4,5,6]
  , [ [0,2,3,6,7,4]
    , [2,0,4,5,8,6]
    , [3,4,0,2,6,5]
    , [6,5,2,0,3,4]
    , [7,8,6,3,0,2]
    , [4,6,5,4,2,0]
    ]
  , [(1,3),(2,3),(3,2),(4,4),(5,5),(6,3)]
  , [(2,5),(1,6)]                  -- exclusiones
  , [(1,2),(2,1),(3,2),(4,3),(5,2),(6,1)] -- prioridades
  , 20
  , 6
  )

domB8 :: DomB
domB8 =
  ( [1,2,3,4,5,6,7,8] 
  , [ [0,2,3,4,6,7,5,4]      -- D(1,_)
    , [2,0,4,3,5,6,8,7]      -- D(2,_)
    , [3,4,0,2,4,5,6,7]      -- D(3,_)
    , [4,3,2,0,3,4,6,5]      -- D(4,_)
    , [6,5,4,3,0,2,3,4]      -- D(5,_)
    , [7,6,5,4,2,0,3,2]      -- D(6,_)
    , [5,8,6,6,3,3,0,2]      -- D(7,_)
    , [4,7,7,5,4,2,2,0]      -- D(8,_)
    ]
  , [ (1,4),(2,3),(3,2),(4,4)
    , (5,5),(6,3),(7,4),(8,2)
    ]                       -- distancia a base r(i)
  , [ (2,5), (3,6)          -- exclusiones: (2,5) y (3,6)
    , (1,7)                 -- no pueden aparecer juntas
    ]
  , [ (1,2),(2,1),(3,2),(4,3)
    , (5,2),(6,1),(7,3),(8,2)
    ]                       -- prioridades
  , 25                      -- M: distancia máxima permitida
  , 6                       -- V: prioridad requerida
  )

domB8_conSol4 :: DomB
domB8_conSol4 =
  ( [1,2,3,4,5,6,7,8]
  , [ [0,2,3,4,5,6,7,4]
    , [2,0,4,3,6,7,4,5]
    , [3,4,0,2,4,6,5,4]
    , [4,3,2,0,3,4,6,5]
    , [5,6,4,3,0,3,4,6]
    , [6,7,6,4,3,0,3,4]
    , [7,4,5,6,4,3,0,2]
    , [4,5,4,5,6,4,2,0]
    ]
  , [ (1,5),(2,5),(3,5),(4,5)
    , (5,8),(6,9),(7,7),(8,6)
    ]
  , [ (1,8),(2,3),(4,7)
    ]   -- exclusiones
  , [ (1,2),(2,2),(3,2),(4,2)
    , (5,2),(6,2),(7,2),(8,2)
    ]
  , 40   -- M: distancia máxima
  , 8    -- V: prioridad mínima (solo alcanzable con ≥4 puntos)
  )

domB8_sinSol :: DomB
domB8_sinSol =
  ( [1,2,3,4,5,6,7,8]
  , [ [0,2,3,4,6,7,5,4]
    , [2,0,4,3,5,6,8,7]
    , [3,4,0,2,4,5,6,7]
    , [4,3,2,0,3,4,6,5]
    , [6,5,4,3,0,2,3,4]
    , [7,6,5,4,2,0,3,2]
    , [5,8,6,6,3,3,0,2]
    , [4,7,7,5,4,2,2,0]
    ]
  , [ (1,10),(2,10),(3,10),(4,10)
    , (5,10),(6,10),(7,10),(8,10)
    ]
  , []     -- sin exclusiones
  , [ (1,1),(2,1),(3,1),(4,1)
    , (5,1),(6,1),(7,1),(8,1)
    ]
  , 10     -- M demasiado bajo
  , 3      -- prioridad mínima (requiere 3 puntos, imposible por distancia)
  )

domB10 :: DomB
domB10 =
  ( [1,2,3,4,5,6,7,8,9,10]
  , [ [0,2,3,4,5,6,7,8,4,5]    -- D(1,_)
    , [2,0,4,3,5,7,6,5,6,7]    -- D(2,_)
    , [3,4,0,2,4,6,7,5,7,6]    -- D(3,_)
    , [4,3,2,0,3,5,6,7,5,6]    -- D(4,_)
    , [5,5,4,3,0,2,3,4,6,7]    -- D(5,_)
    , [6,7,6,5,2,0,3,4,5,6]    -- D(6,_)
    , [7,6,7,6,3,3,0,2,3,5]    -- D(7,_)
    , [8,5,5,7,4,4,2,0,3,2]    -- D(8,_)
    , [4,6,7,5,6,5,3,3,0,2]    -- D(9,_)
    , [5,7,6,6,7,6,5,2,2,0]    -- D(10,_)
    ]
  , [ (1,4),(2,4),(3,3),(4,4),(5,5)
    , (6,4),(7,4),(8,3),(9,5),(10,4)
    ]                           -- distancia a la base r(i)
  , [ (2,7), (4,9), (3,10)       -- exclusiones moderadas
    ]
  , [ (1,2),(2,2),(3,2),(4,2),(5,2)
    , (6,2),(7,2),(8,2),(9,2),(10,2)
    ]                           -- prioridades uniformes
  , 40                          -- M: distancia máxima
  , 6                           -- V: prioridad mínima
  )
