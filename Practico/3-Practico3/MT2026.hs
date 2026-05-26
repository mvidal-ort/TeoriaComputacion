-- Martin Vidal 68694 - Copiado de la tarea MT.hs

{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module MT2026 where

data Symbol = S String | WC

instance Eq Symbol where -- voy a usar el lookup: el lookup usa el == para comparar
  (S s) == (S s') = s == s'
  (S s) == WC = True
  WC == (S s) = True
  WC == WC = True

instance Show Symbol where -- para que quede mas prolijo el codigo 
  show (S s) = s
  show WC = "_"

type Tape = ([Symbol], Symbol, [Symbol])

type State = String

data Action = L | R | W Symbol deriving (Show, Eq)

type Code = [(State, [Branch])]

type Branch = (Symbol, (Action, State))

exec :: Code -> Tape -> Tape
exec code tape = fullExec "i" code tape

fullExec :: State -> Code -> Tape -> Tape
fullExec q code tape =
  if q == "h"
    then tape
    else fullExec q' code tape'
  where
    (tape', q') = step code tape q

step :: Code -> Tape -> State -> (Tape, State)
step code (l, s, r) q = case lookup q code of
  Just bs -> case lookup s bs of
    Just (a, q') -> case a of
      L -> ((init' l, last' l, s : r), q')
      R -> ((l ++ [s], head' r, tail' r), q')
      W s' -> ((l, s', r), q')

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
tail' [] = []
tail' xs = tail xs

nott :: Code
nott =
  [ ( "i",
      [ (S "F", (W (S "T"), "h")),
        (S "T", (W (S "F"), "h"))
      ]
    )
  ]

tapeNott :: ([a1], Symbol, [a2])
tapeNott = ([], S "T", [])

resNot :: Tape
resNot = exec nott tapeNott

lSigma :: String -> Code
lSigma s =
  [ ( "i",
      [ (WC, (L, "loop"))
      ]
    ),
    ( "loop",
      [ (S s, (W (S s), "h")),
        (WC, (L, "loop"))
      ]
    )
  ]

tapelSigma :: Tape
tapelSigma = ([S "T",S "#", S "D", S "#", S "A", S "B", S "C"], S "T", [])

-----------------

parMT :: Code
parMT =
  [ ("i",
      [ (S "#", (L, "par"))
      ]
    ),

    ("par",
      [ (S "X1", (L, "impar")),
        (S "#", (R, "volverP"))
      ]
    ),

    ("impar",
      [ (S "X1", (L, "par")),
        (S "#", (R, "volverI"))
      ]
    ),

    ("volverP",
      [ (S "X1", (R, "volverP")),
        (S "#", (R, "writeP"))
      ]
    ),

    ("volverI",
      [ (S "X1", (R, "volverI")),
        (S "#", (R, "writeI"))
      ]
    ),

    ("writeP",
      [ (WC, (W (S "P"), "h"))
      ]
    ),

    ("writeI",
      [ (WC, (W (S "I"), "h"))
      ]
    )
  ]


elemSigma :: String -> Code
elemSigma s =
  [ ("i",
      [ (S "#", (L, "buscar"))
      ]
    ),

    ("buscar",
      [ (S s, (R, "volverY")),
        (S "#", (R, "volverN")),
        (WC, (L, "buscar"))
      ]
    ),

    ("volverY",
      [ (S "a", (R, "volverY")),
        (S "b", (R, "volverY")),
        (S "#", (R, "writeY"))
      ]
    ),

    ("volverN",
      [ (S "a", (R, "volverN")),
        (S "b", (R, "volverN")),
        (S "#", (R, "writeN"))
      ]
    ),

    ("writeY",
      [ (WC, (W (S "Y"), "h"))
      ]
    ),

    ("writeN",
      [ (WC, (W (S "N"), "h"))
      ]
    )
  ]


-- ===============--
--- Para probar---
-- ===============--
m1 :: Code
m1 =
  [ ("i", [(WC, (W (S "x"), "h"))])
  ]

t1 :: Tape
t1 = ([], S "a", [])

-----------
m2 :: Code
m2 =
  [ ("i", [(WC, (R, "h"))])
  ]

t2 :: Tape
t2 = ([S "a"], S "b", [S "c", S "d"])

------------
t3 :: Tape
t3 =
  ([S "a", S "b"], S "c", [S "d"])

-------------
tPar :: Tape
tPar =
  ([S "X1", S "X1", S "X1", S "X1"],
   S "#",
   [S "#"])

tImpar :: Tape
tImpar =
  ([S "X1", S "X1", S "X1"],
   S "#",
   [S "#"])

----------------

tapeElemY :: Tape
tapeElemY =
  ([S "b", S "a", S "b", S "a"],
   S "#",
   [S "#"])

tapeElemN :: Tape
tapeElemN =
  ([S "b", S "b", S "b"],
   S "#",
   [S "#"])