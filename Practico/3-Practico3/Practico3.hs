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

instance Show Symbol where -- para que quede mas prolijo el codigo 
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

exec :: Code -> Tape -> Tape
iter :: Code -> Config -> Config
step :: Tape -> [Branch]-> Config
---------


-- exec :: Code -> Tape -> Tape
-- exec code tape = fullExec "i" code tape

-- fullExec :: State -> Code -> Tape -> Tape
-- fullExec q code tape =
--   if q == "h"
--     then tape
--     else fullExec q' code tape'
--   where
--     (tape', q') = step code tape q

-- step :: Code -> Tape -> State -> (Tape, State)
-- step code (l, s, r) q = case lookup q code of
--   Just bs -> case lookup s bs of
--     Just (a, q') -> case a of
--       L -> ((init' l, last' l, s : r), q')
--       R -> ((l ++ [s], head' l, tail' l), q')
--       W s' -> ((l, s', r), q')

-- Estas funciones del preludio dan error con listas vacías, cuidado
-- init' :: [Symbol] -> [Symbol]
-- init' [] = []
-- init' xs = init xs

-- last' :: [Symbol] -> Symbol
-- last' [] = S "#"
-- last' xs = last xs

-- head' :: [Symbol] -> Symbol
-- head' [] = S "#"
-- head' xs = head xs

-- tail' :: [Symbol] -> [Symbol]
-- tail' [] = [S "#"]
-- tail' xs = tail xs