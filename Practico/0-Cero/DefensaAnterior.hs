{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module DefensaAnterior where

import Data.List (subsequences)

-- Evaluador de expresiones para matrices 2x2 que prefixRelativeComponentInstallDirs
-- - Variables 
-- - Asignaciones 
-- - una matriz 2x2 con coeficientes enteros a, b, c, d
-- - escalado de matrices: k*m, donde m es una matriz 2x2 y k un entero 
-- - suma y resta de expresiones matriciales 
-- - traza de una matriz 
-- - transpuesta de una matriz 
-- - determinar si una matriz es diagonal

data E = 
    Var X          -- variable
    | Assig X E      -- asignación
    | MT Mat          -- matriz literal 2x2
    | Scale Z E      -- escalado: k * M
    | Add E E        -- suma de matrices
    | Sub E E        -- resta de matrices
    | Trace E        -- traza
    | Transpose E    -- transpuesta
    | IsDiagonal E   -- ¿es diagonal?
    deriving Show

type X = String
type Z = Int

data Mat = Mat Z Z Z Z deriving Show

data V = VM Mat | VI Z | VB Bool

type M = [(X,V)]

-- Auxiliares:
lkup :: X -> M -> V
lkup x [] = error "Variable no definida"
lkup x ((x',v):m) 
    | x == x' = v
    | otherwise =  lkup x m

upd :: M -> (X,V) -> M
upd [] (x0,v0) = [(x0,v0)]
upd ((x,v):xs) (x0,v0) 
    | (x == x0) = (x0,v0):xs
    | otherwise = (x,v):(upd xs (x0,v0))

------------------------


eval :: M -> E -> (M,V)

eval m (Var x) = (m,lkup x m)

eval m (Assig x e) = 
    let (m', v) = eval m e
    in (upd m' (x,v), v)

eval m (MT matriz) =



