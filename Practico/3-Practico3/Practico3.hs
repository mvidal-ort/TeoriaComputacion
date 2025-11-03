{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Practico3 where

data Symbol = S String | WC

blank :: Symbol
blank = S "#"

palo :: Symbol
palo = S "|"

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
    Nothing -> error "No hay transición para el símbolo actual"

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
                (WC, (R, "buscar"))]),  -- <- por seguridad

    ("volver",  [(S "1", (L, "volver")),
                 (S "X", (W (S "1"), "h"))])
  ]

tapeCopiar :: Tape
tapeCopiar = ([], S "1", [S "1", S "1", S "#"])
---------------------------
-- Lσ: que dada una tira de s´ımbolos sobre el alfabeto Σ = {σ1, σ2, σ3}, se mueve estrictamente
-- a la izquierda hasta encontrarse con el s´ımbolo σ,
codeLsigma :: Symbol -> Code
codeLsigma sigma =
  [ ("i", [(sigma, (W sigma, "h")),   -- si encuentra σ, se detiene
           (WC, (L, "i"))])           -- si no, sigue moviéndose a la izquierda
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
