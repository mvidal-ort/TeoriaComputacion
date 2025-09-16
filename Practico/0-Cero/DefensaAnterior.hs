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
    | Matriz Mat     -- matriz literal 2x2
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

scaleMat :: Int -> Mat -> Mat
scaleMat k (Mat a b c d) = Mat (k*a) (k*b) (k*c) (k*d)

addMat :: Mat -> Mat -> Mat
addMat (Mat a b c d) (Mat a' b' c' d') = Mat (a+a') (b+b') (c+c') (d+d')

substMat :: Mat -> Mat -> Mat
substMat (Mat a b c d) (Mat a' b' c' d') = Mat (a-a') (b-b') (c-c') (d-d')

traceMat :: Mat -> Int
traceMat (Mat a b c d) = a+b

transposeMat :: Mat -> Mat
transposeMat (Mat a b c d) = Mat a c b d

isDiagonalMat :: Mat -> Bool
isDiagonalMat (Mat a b c d) = b == 0 && c == 0
------------------------


eval :: M -> E -> (M,V)

eval m (Var x) = (m,lkup x m)

eval m (Assig x e) = 
    let (m', v) = eval m e
    in (upd m' (x,v), v)

eval m (Matriz matriz) = (m, VM matriz)

-- eval m (Scale k e) = 
--     let (m', v) = eval m e in case v of 
--         VM mat -> (m', VM (scaleMat k mat)) 
--         _ -> error "Scale aplicado a no-matriz"
-- -- Segura porque verifca el tipo del valor obtenido en eval en tiempo de ejecucion

eval m (Scale k e) =
    let (m', VM v) = eval m e
    in (m', VM (scaleMat k v))

eval m (Add e1 e2) =
    let (m', VM v1) = eval m e1
        (m'', VM v2) = eval m' e2    
    in (m'', VM (addMat v1 v2))

eval m (Sub e1 e2) =
    let (m', VM v1) = eval m e1
        (m'', VM v2) = eval m' e2    
    in (m'', VM (substMat v1 v2))

eval m (Trace e) =
    let (m', VM v) = eval m e
    in (m', VI (traceMat v))

eval m (Transpose e) =
    let (m', VM v) = eval m e
    in (m', VM (transposeMat v))

eval m (IsDiagonal e) =
    let (m', VM v) = eval m e
    in (m', VB (isDiagonalMat v))
