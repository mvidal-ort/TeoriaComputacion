-- Martin Vidal 68694

{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Tarea3 where
import Debug.Trace (trace)
-- Use el trace para poder debuggear ya que no estaba obteniendo los resultados esperados
-- Igualmente, veo que hay ejemplos en los que fallan las funciones)

--1. Definir tipos apropiados para representar:
--Simbolos
data Symb = Blanco -- #
    | Sigma String
    | Comodin
    deriving (Show, Eq)
    
--Estados
data Q = I 
    | H 
    | E String 
    deriving Show   

--Cintas
type L = [Symb] --Cinta Izquierda
type R = [Symb] --Cinta Derecha
type S = Symb --Símbolo Corriente
type Cinta = (L,S,R)

--Acciones
data A = Mov_izq 
    | Mov_der 
    | Write Symb 
    deriving Show

--Código.
type B = (Symb,A,Q)
type M = [(Q,[B])]


--2. Definir la función (parcial) de ejecución de un código sobre una cinta dada, con las funciones auxiliares que sean necesarias.
exec :: Cinta -> M -> Cinta
exec cinta codigo = fst(itera cinta codigo I)

instance Eq Q where
	(==) = \n m -> case n of {
		I -> case m of {I -> True; otherwise -> False};
		H -> case m of {H -> True; otherwise -> False};
        E estado -> case m of {(E estado2) -> if (estado == estado2) then True else False; otherwise -> False}
    }

ramas :: M -> Q -> [B]
ramas [] e = []
ramas ((e1, b) : resto) e2
    | (e1 == e2) = b
    | otherwise = ramas resto e2

						
itera :: Cinta -> M -> Q -> (Cinta, Q)
itera cinta codigo H =
    trace ("Estado: H\nCinta: " ++ show cinta)
    (cinta, H)
itera cinta codigo estado =
    trace ("Estado: " ++ show estado ++ "\nCinta: " ++ show cinta)
    (itera cinta' codigo estado')
  where
    (cinta', estado') = paso cinta (ramas codigo estado)
		

lkup s [] = error $ "Error: No se encuentra el símbolo " ++ show s ++ " en la lista de ramas."
lkup s ((sym, action, estado) : resto)
    | s == sym = (action, estado)
    | sym == Comodin && null (filter (\(x, _, _) -> x == s) resto) = (action, estado)
    | otherwise = lkup s resto


paso ([], s, der) rs = case lkup s rs of
    (Mov_izq, estado) -> (([Blanco], Blanco, s : der), estado)
    (Mov_der, estado) -> if null der
        then (([], Blanco, []), H)
        else (([], head der, tail der), estado)
    (Write simbolo, estado) -> (([], simbolo, der), estado)
paso (izq, s, der) rs = case lkup s rs of
    (Mov_izq, estado) -> if null izq
        then (([Blanco], Blanco, s : der), estado)
        else ((init izq, last izq, s : der), estado)
    (Mov_der, estado) -> if null der
        then ((izq, Blanco, []), H)
        else ((izq ++ [s], head der, tail der), estado)
    (Write simbolo, estado) -> ((izq, simbolo, der), estado)


--3. Codificar MT embebidas en Haskell que computen los programas:
--Lσ: que dada una tira de s´ımbolos sobre el alfabeto Σ = {σ1, σ2,σ3}, se mueve estrictamente a la izquierda hasta encontrarse con el
--s´ımbolo σ, siguiendo el ejemplo dado en la especificaci´on.

lsig :: String -> M
lsig sigma = 
    [(I, 
        [(Comodin, Mov_izq, E "Loop"), 
         (Sigma sigma, Write (Sigma sigma), H)]), 
     (E "Loop", 
        [(Sigma sigma, Write (Sigma sigma), H), 
         (Comodin, Mov_izq, E "Loop"), 
         (Blanco, Write Blanco, H)])] 
	

-- Par: que dada una tira de s´ımbolos sobre el alfabeto Σ = {σ1}, determina si una tira de s´ımbolos tiene largo par o no.
par :: M
par =
    [ (I, 
        [(Comodin, Mov_der, E "Loop"), 
         (Blanco, Write (Sigma "T"), H)]), 
      (E "Loop", 
        [(Comodin, Mov_der, E "Loop"),
         (Blanco, Mov_izq, E "Check")]), 
      (E "Check", 
        [(Comodin, Mov_izq, E "Impar"), 
         (Blanco, Mov_der, E "Termina")]), 
      (E "Impar",
        [(Blanco, Write (Sigma "F"), H),
         (Comodin, Mov_izq, E "Par")]), 
      (E "Par",
        [(Blanco, Write (Sigma "T"), H),
         (Comodin, Mov_izq, E "Impar")])]


--Elemσ: que dada una tira de s´ımbolos sobre el alfabeto Σ = {σ1,σ2} y un s´ımbolo σ, determina si el s´ımbolo aparece en la palabra.
-- elemMT para no tener conflicto con elem de Prelude
elemMT :: String -> M
elemMT caracter = 
    [ (I,
        [(Comodin, Mov_izq, E "Loop_buscar")]),
      (E "Loop_buscar",
        [(Blanco, Mov_der, E "Loop_no_encontrado"),
         (Sigma caracter, Mov_der, E "Loop_encontrado"),
         (Comodin, Mov_izq, E "Loop_buscar")]),
      (E "Loop_no_encontrado",
        [(Blanco, Write (Sigma "F"), H), -- Escribe F y se detiene
         (Comodin, Mov_der, E "Loop_no_encontrado")]),
      (E "Loop_encontrado",
        [(Blanco, Write (Sigma "T"), H), -- Escribe T y se detiene
         (Comodin, Mov_der, E "Loop_encontrado")])]

-- Reverse: que dada una tira de s´ımbolos sobre el alfabeto Σ = {σ1,σ2}, la devuelve invertida y separada por un blanco (#) de la palabra original.
reverseMT :: M
reverseMT =
    [ (I, -- Estado inicial: mover al final de la cinta
        [(Comodin, Mov_der, E "Mover_derecha")]),
      (E "Mover_derecha", -- Mueve hacia la derecha hasta encontrar Blanco
        [(Blanco, Mov_izq, E "Procesar"),
         (Comodin, Mov_der, E "Mover_derecha")]),
      (E "Procesar", -- Procesa cada símbolo desde el final hacia el principio
        [(Blanco, Write (Sigma "#"), H), -- Escribe el separador `#` y termina
         (Sigma "a", Write (Sigma "X"), E "Mover_a_inicio"),
         (Sigma "b", Write (Sigma "Y"), E "Mover_a_inicio")]),
      (E "Mover_a_inicio", -- Mueve al principio de la cinta invertida
        [(Blanco, Mov_izq, E "Escribir_invertido"),
         (Comodin, Mov_izq, E "Mover_a_inicio")]),
      (E "Escribir_invertido", -- Escribe el símbolo procesado en la cinta invertida
        [(Sigma "X", Write (Sigma "a"), E "Volver_a_procesar"),
         (Sigma "Y", Write (Sigma "b"), E "Volver_a_procesar")]),
      (E "Volver_a_procesar", -- Vuelve al siguiente símbolo para continuar procesando
        [(Comodin, Mov_der, E "Procesar")])]

