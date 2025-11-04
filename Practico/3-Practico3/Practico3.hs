{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico3 where

data Symbol = S String | WC

blank :: Symbol
blank = S "#"



instance Eq Symbol where -- voy a usar el lookup: el lookup usa el == para comparar
  (S s) == (S s') = s == s'
  (S s) == WC = True
  WC == (S s) = True
  WC == WC = True

instance Show Symbol where -- para ver la cinta sin los constructores
  show (S s) = s
  show WC = "_"

type Tape = ([Symbol], Symbol, [Symbol])

type State = String

inicio :: State
inicio = "i"
halt :: State
halt = "h"

data Action = L | R | W Symbol deriving (Show)

type Code = [(State, [Branch])]

type Branch = (Symbol, (Action, State))

type Config = (State, Tape) 


step :: Tape -> [Branch] -> Config
step (l, s, r) bs =
  case lookup s bs of
    Just (a, q') ->
      case a of
        L     -> (q', (init' l, last' l, s : r))
        R     -> (q', (l ++ [s], head' r, tail' r))
        W s'  -> (q', (l, s', r))
    Nothing -> error "No hay transición para el símbolo actual "

iter :: Code -> Config -> Config
iter code (q, tape)
  | q == halt = (q, tape)  -- caso base: máquina detenida
  | otherwise =
      case lookup q code of
        Nothing -> error "No hay código para este estado" 
        Just bs -> iter code (step tape bs)

exec :: Code -> Tape -> Tape
exec code tape = snd (iter code (inicio, tape))

-- Estas funciones del preludio dan error con listas vacías, cuidado
init' :: [Symbol] -> [Symbol]
init' [] = []
init' xs = init xs

last' :: [Symbol] -> Symbol
last' [] = S "#"
last' xs = last xs

head' :: [Symbol] -> Symbol
head' [] = S "#"
head' xs = head xs

tail' :: [Symbol] -> [Symbol]
tail' [] = [S "#"]
tail' xs = tail xs

-----------------------
codeEjemplo :: Code
codeEjemplo =
  [ ("i", [(S "1", (W (S "0"), "h"))]) ]  -- escribe 0 sobre 1 y se detiene

tape0 :: Tape
tape0 = ([], S "1", [])
-------------------------
codeMover :: Code
codeMover =
  [ ("i", [(S "1", (R, "i")),   -- mientras lea '1', mover a la derecha
            (S "#", (W (S "#"), "h"))]) -- si lee '#', se detiene
  ]

tapeMover :: Tape
tapeMover = ([], S "1", [S "1", S "1", S "#"])
---------------------------
codeCopiar :: Code
codeCopiar =
  [ ("i",       [(S "1", (W (S "X"), "buscar")),
                 (S "#", (W (S "#"), "h"))]),

   ("buscar", [(S "1", (R, "buscar")),
                (S "#", (W (S "1"), "volver")),
                (WC, (R, "buscar"))]),  

    ("volver",  [(S "1", (L, "volver")),
                 (S "X", (W (S "1"), "h"))])
  ]

tapeCopiar :: Tape
tapeCopiar = ([], S "1", [S "1", S "1", S "#"])
---------------------------
-- Lσ: que dada una tira de siımbolos sobre el alfabeto Σ = {σ1, σ2, σ3}, se mueve estrictamente
-- a la izquierda hasta encontrarse con el siımbolo σ,
codeLsigma :: Symbol -> Code
codeLsigma sigma =
  [ ("i", [(sigma, (W sigma, "h")),   
           (WC, (L, "i"))])          
  ]

codeLB :: Code
codeLB = codeLsigma sigma

tapeLB :: Tape
tapeLB = ([S "A", S "B", S "C", S "C"], S "C", [S "A"])

sigma :: Symbol -- Para probar con B
sigma = S "B"
----------------------------
-- Par: que dada una tira de sımbolos sobre el alfabeto Σ = {σ1}, determina si una tira de
-- sımbolos tiene largo par o no.

palo :: Symbol
palo = S "|"

codeParidad :: Code
codeParidad =
  [ ("i", [(S "#", (L, "q0"))])
  , ("q0",[(palo, (L, "q0")),
           (S "#", (W (S "#"), "q1"))])
  , ("q1", [(S "#", (R, "q2"))])
  , ("q2",[(palo, (R, "q5")),
           (S "#", (R, "q3"))])
  , ("q5",[(palo, (R, "q2")),
           (S "#", (R, "q6"))])
  , ("q6", [(S "#", (W (S "F"), "q4"))])
  , ("q3", [(S "#", (W (S "T"), "q4"))])
  , ("q4", [(WC, (R, "h"))])

  ]


tapeParVacia :: Tape
tapeParVacia = ([S "#"], S "#", [])   

tape4 :: Tape
tape4 = ([S "#", palo, palo, palo, palo], S "#", [])

tape3 :: Tape
tape3 = ([S "#", palo, palo, palo], S "#", [])

tape11 :: Tape
tape11 = ([S "#", palo, palo, palo,palo, palo, palo,palo, palo, palo], S "#", [])

------------------------------------
-- Elemσ: que dada una tira de siımbolos sobre el alfabeto Σ = {σ1, σ2} y un siımbolo σ, determina
-- si el siımbolo aparece en la palabra.

-- Para ver si aparece S1
codeElem :: Code
codeElem =
  [ ("i", [(blank, (L, "q0"))])
  , ("q0",[(sig1, (L, "q1"))])
  , ("q1", [(blank, (L, "q2"))])
  , ("q2",[(sig1, (R, "q3")),
           (sig2, (L, "q2")),
           (blank, (R, "q8"))])
  , ("q3",[(sig2, (R, "q3")),
           (blank, (R, "q4"))])
  , ("q4", [(sig1, (R, "q5"))])
  , ("q5", [(blank, (R, "q6"))])
  , ("q6", [(blank, (W (S "T"), "q7"))])
  , ("q7", [(WC, (R, "h"))])
  , ("q8",[(sig2, (R, "q8")),
           (blank, (R, "q9"))])
  , ("q9", [(sig1, (R, "q10"))])
  , ("q10", [(blank, (R, "q11"))])
  , ("q11", [(blank, (W (S "F"), "q7"))])

  ]

sig1 :: Symbol 
sig1 = S "S1"

sig2 :: Symbol 
sig2 = S "S2"

tapeSigmaF :: Tape
tapeSigmaF = ([S "#", sig2, sig2, sig2, sig2, S "#", sig1], S "#", [S "#"])

tapeSigmaT :: Tape
tapeSigmaT = ([S "#", sig2, sig1, sig2, sig2, S "#", sig1], S "#", [S "#"])

tapeSigmaT2 :: Tape
tapeSigmaT2 = ([S "#", sig1, sig2, sig2, sig2, S "#", sig1], S "#", [S "#"])

tapeSigmaT3 :: Tape
tapeSigmaT3 = ([S "#", sig2, sig2, sig2, sig1, S "#", sig1], S "#", [S "#"])
---------------
-- Reverse: que dada una tira de s´ımbolos sobre el alfabeto Σ = {σ1, σ2}, la devuelve invertida
-- y separada por un blanco (#) de la palabra original.

codeReverse :: Code
codeReverse =
  [ ("i", [(blank, (L, "q0"))])
  , ("q0",[(sig1, (W (S"X"), "q1")),
           (sig2, (W (S "X"), "q5")),
           (blank, (R, "q8"))])
  , ("q1", [(blank, (R, "q2")),
            (WC, (R, "q1"))])
  , ("q2", [(blank, (W sig1, "q3")),
            (WC, (R, "q2"))])
  , ("q3",[(S "X", (W sig1, "q4")),
           (WC, (L, "q3"))])
  , ("q4", [(WC, (L, "q0"))])
  , ("q5",[(blank, (R, "q6")),
           (WC, (R, "q5"))])
  , ("q6",[(blank, (W sig2, "q7")),
           (WC, (R, "q6"))])
  , ("q7",[(S "X", (W sig2, "q4")),
           (WC, (L, "q7"))])
  , ("q8",[(blank, (W blank, "h")),
           (WC, (R, "q8"))])
  
  ]

tapeReverse :: Tape
tapeReverse = ([S "#", sig2, sig2, sig2, sig1,sig2, sig2,sig1, sig1], S "#", [S "#"])
--tapeReverse = ([S "#", sig1], S "#", [S "#", S "#"])